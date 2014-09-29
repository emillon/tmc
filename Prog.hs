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
            ) where

import Control.Monad.Free

data AudioType = Mp3
               | Flac
    deriving (Show)

-- | Some Audio tracks have BPM, others do not.
data Audio = Audio { aPath :: FilePath
                   , aBPM :: Maybe Double
                   }
    deriving (Show)

data ProgF a = File Track (Audio -> a)
             | Bind Op (Audio -> a)

data Op = OpSoxFX SoxFX Audio
        | Merge Audio Audio

instance Functor ProgF where
    fmap f (File track k) = File track (f . k)
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
opBPM (OpSoxFX (SoxTempo ratio) a) = do
    bpm <- aBPM a
    return $ ratio * bpm
opBPM (OpSoxFX (SoxPad _) a) = aBPM a
opBPM (OpSoxFX (SoxGain _) a) = aBPM a
opBPM (Merge a _b) = aBPM a -- we assume that we're mixing similar tracks
