module Prog ( Prog
            , ProgF(..)
            , Audio(..)
            , AudioType(..)
            , Op(..)
            , SoxFX
            , soxCompile
            , soxFX
            , warpAudio
            , shiftAudio
            , gainAudio
            , mergeAudio
            , Track(..)
            , audioTrack
            , opBPM
            , opStart
            , opCo
            ) where

import Control.Applicative
import Control.Monad.Free

import Cache

data AudioType = Mp3
               | Flac
    deriving (Show)

-- | Some Audio tracks have BPM, others do not.
--   Same for start time.
data Audio = Audio { aCache :: CObject
                   , aPath :: FilePath
                   , aBPM :: Maybe Double
                   , aStart :: Maybe Double
                   }
    deriving (Show)

data ProgF a = File Track (Audio -> a)
             | Synth Double Double (Audio -> a)
             | Bind Op (Audio -> a)

data Op = OpSoxFX SoxFX Audio
        | Merge Audio Audio
        | Sequence Audio Audio

instance Functor ProgF where
    fmap f (File track k) = File track (f . k)
    fmap f (Synth freq dur k) = Synth freq dur (f . k)
    fmap f (Bind op k) = Bind op (f . k)

type Prog a = Free ProgF a

data SoxFX = SoxTempo Double
           | SoxPad Double
           | SoxGain Double

soxCompile :: SoxFX -> [String]
soxCompile (SoxTempo ratio) = ["tempo", show ratio]
soxCompile (SoxPad amount) = ["pad", show amount]
soxCompile (SoxGain amount) = ["gain", show amount]

soxFX :: SoxFX -> Audio -> Prog Audio
soxFX fx a = liftF $ Bind (OpSoxFX fx a) id

warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ SoxTempo ratio

shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = soxFX $ SoxPad amount

gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = soxFX $ SoxGain amount

mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Bind (Merge a b) id

data Track = Track { trackFormat :: AudioType
                   , trackPath :: FilePath
                   , trackBPM :: Double
                   , trackStart :: Double
                   }

audioTrack :: Track -> Prog Audio
audioTrack t = liftF $ File t id

opBPM :: Op -> Maybe Double
opBPM (OpSoxFX sfx a) = soxBPM sfx <$> aBPM a
opBPM (Merge a _b) = aBPM a -- we assume that we're mixing similar tracks
opBPM (Sequence _ _) = Nothing -- could optimize when a & b are close

soxBPM :: SoxFX -> Double -> Double
soxBPM (SoxTempo ratio) x = ratio * x
soxBPM (SoxPad _) x = x
soxBPM (SoxGain _) x = x

opStart :: Op -> Maybe Double
opStart (OpSoxFX sfx a) = soxStart sfx <$> aStart a
opStart (Merge a _b) = aStart a -- we assume that we're mixing aligned tracks
opStart (Sequence a _b) = aStart a

soxStart :: SoxFX -> Double -> Double
soxStart (SoxTempo ratio) x = ratio * x
soxStart (SoxPad shift) x = shift + x
soxStart (SoxGain _) x = x

coMake :: String -> [Audio] -> CObject
coMake op deps =
    CObject { coOp = op
            , coDeps = map (coHash . aCache) deps
            }

opCo :: Op -> CObject
opCo (OpSoxFX sfx a) = coMake ("SoxFX (" ++ unwords (soxCompile sfx) ++ ")") [a]
opCo (Merge a b) = coMake "Merge" [a, b]
opCo (Sequence a b) = coMake "Sequence" [a, b]
