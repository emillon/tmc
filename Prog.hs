module Prog ( Prog
            , ProgF(..)
            , Audio(..)
            , AudioType(..)
            , Source(..)
            , Op(..)
            , SoxFX
            , soxCompile
            , showD
            , soxFX
            , warpAudio
            , shiftAudio
            , gainAudio
            , mergeAudio
            , synth
            , silence
            , sequenceAudio
            , seqList
            , Track(..)
            , audioTrack
            , opBPM
            , opStart
            , opCo
            , metronome
            , checkBPM
            ) where

import Control.Applicative
import Control.Monad.Free
import Text.Printf

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

data ProgF a = Source Source (Audio -> a)
             | Bind Op (Audio -> a)

data Op = OpSoxFX SoxFX Audio
        | Merge Audio Audio
        | Sequence Audio Audio

data Source = File Track
            | Synth Double Double
            | Silence Double

instance Functor ProgF where
    fmap f (Source src k) = Source src(f . k)
    fmap f (Bind op k) = Bind op (f . k)

type Prog a = Free ProgF a

data SoxFX = SoxTempo Double
           | SoxPad Double
           | SoxGain Double

soxCompile :: SoxFX -> [String]
soxCompile (SoxTempo ratio) = ["tempo", showD ratio]
soxCompile (SoxPad amount) = ["pad", showD amount]
soxCompile (SoxGain amount) = ["gain", showD amount]

showD :: Double -> String
showD d = printf "%f" d

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
audioTrack t = liftF $ (Source $ File t) id

synth :: Double -> Double -> Prog Audio
synth freq dur = liftF $ (Source $ Synth freq dur) id

silence :: Double -> Prog Audio
silence dur = liftF $ (Source $ Silence dur) id

sequenceAudio :: Audio -> Audio -> Prog Audio
sequenceAudio a b = liftF $ Bind (Sequence a b) id

seqList :: [Audio] -> Prog Audio
seqList [] = error "seqList"
seqList [a] = return a
seqList (a:as) = do
    r <- seqList as
    sequenceAudio a r

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

metronome :: Double -> Int -> Prog Audio
metronome bpm nbars = do
    hiBeep <- synth hiFreq beepLen
    sil <- silence silenceLen
    loBeep <- synth loFreq beepLen
    bar <- seqList [hiBeep, sil, loBeep, sil, loBeep, sil, loBeep, sil]
    seqList $ replicate nbars bar
        where
            beepLen = 0.1
            hiFreq = 2 * loFreq
            loFreq = 440
            silenceLen = beatLen - beepLen
            beatLen = 60 / bpm

checkBPM :: Track -> Prog Audio
checkBPM track = do
    a <- audioTrack track
    m <- metronome (trackBPM track) 16
    s <- shiftAudio (trackStart track) m
    mergeAudio a s
