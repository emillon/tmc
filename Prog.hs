-- | The DSL itself.

module Prog ( -- * Audio Tracks
              Audio
            , aBPM
            , aStart
              -- * The DSL
              -- ** Type
            , Prog
              -- ** Sources
            , Track(..)
            , AudioType(..)
            , audioTrack
            , synth
            , silence
              -- ** Operations
            , warpAudio
            , shiftAudio
            , gainAudio
            , mergeAudio
            , sequenceAudio
            , seqList
              -- * Tools
            , metronome
            , checkBPM
              -- * Interpreter
            , run
            , steps
            ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import System.Cmd
import Text.Printf

import Cache

-- | Type of an audio source
data AudioType = Mp3
               | Flac
    deriving (Show)

-- | A bounced audio track.
-- This corresponds to an intermediate WAV file with
-- metadata such as BPM or start time.
data Audio = Audio { aCache :: CObject
                   , aPath :: FilePath
                   , aBPM :: Maybe Double -- ^ Retrieve the BPM of an audio track
                   , aStart :: Maybe Double -- ^ Retrieve the start time of an audio track
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

-- | Our main Monad.
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

-- | Change the speed of a track without altering its pitch
warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ SoxTempo ratio

-- | Pad with silence before the start.
shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = soxFX $ SoxPad amount

-- | Adjust the gain (in DBs).
gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = soxFX $ SoxGain amount

-- | Mix tracks together.
mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Bind (Merge a b) id

-- | An audio file on disk. 'trackStart' is the position of beat zero.
data Track = Track { trackFormat :: AudioType
                   , trackPath :: FilePath
                   , trackBPM :: Double
                   , trackStart :: Double
                   }

-- | Read a 'Track'.
audioTrack :: Track -> Prog Audio
audioTrack t = liftF $ (Source $ File t) id

-- | Generate a sine wave.
synth :: Double -- ^ Frequency
      -> Double -- ^ Duration
      -> Prog Audio
synth freq dur = liftF $ (Source $ Synth freq dur) id

-- | Generate silence.
silence :: Double -- ^ Duration
        -> Prog Audio
silence dur = liftF $ (Source $ Silence dur) id

-- | Join tracks (play one after another).
sequenceAudio :: Audio -> Audio -> Prog Audio
sequenceAudio a b = liftF $ Bind (Sequence a b) id

-- | An extension of 'sequenceAudio' to lists.
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

-- | A track that only does beep beep.
-- There are actually two different beeps, a high one and a low one.
-- It does H L -- L L H L L L... (4/4)
metronome :: Double -- ^ BPM
          -> Int -- ^ Number of bars
          -> Prog Audio
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

-- | Play a metronome synchronized on top of a track.
-- It should be an easy way to know if the track's metadata is correct.
checkBPM :: Track -> Prog Audio
checkBPM track = do
    a <- audioTrack track
    m <- metronome (trackBPM track) 16
    s <- shiftAudio (trackStart track) m
    mergeAudio a s

execCommand :: String -> [String] -> IO ()
execCommand cmd args =
    void $ rawSystem cmd args

interpretOp :: Op -> FilePath -> IO ()
interpretOp (OpSoxFX fx (Audio _ input _ _)) temp =
    execCommand "sox" $ [input, temp] ++ soxCompile fx
interpretOp (Merge a b) temp =
    execCommand "sox" ["-m", aPath a, aPath b, temp]
interpretOp (Sequence a b) temp =
    execCommand "sox" [aPath a, aPath b, temp]

decodeFile :: AudioType -> FilePath -> FilePath -> IO ()
decodeFile Mp3 input output =
    execCommand "lame" ["--decode", input, output]
decodeFile Flac input output =
    execCommand "flac" ["-f", "--decode", "--no-preserve-modtime", input, "-o", output]

genSynth :: Double -> Double -> FilePath -> IO ()
genSynth freq dur output =
    execCommand "sox" ["-n", "-r", "44100", output, "synth", showD dur, "sine", showD freq]

genSilence :: Double -> FilePath -> IO ()
genSilence dur output =
    execCommand "sox" ["-n", "-r", "44100", output, "trim", "0", showD dur]

-- | Execute the program: invoke tools that actually do the manipulation.
run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (Source (File track) k)) = do
    let fmt = trackFormat track
        path = trackPath track
        bpm = trackBPM track
        start = trackStart track
        co = CObject { coOp = "File"
                     , coDeps = []
                     }
    temp <- cached co $ decodeFile fmt path
    run $ k $ Audio co temp (Just bpm) (Just start)
run (Free (Source (Synth freq dur) k)) = do
    let co = CObject { coOp = "Synth (" ++ show freq ++ ", " ++ show dur ++ ")"
                     , coDeps = []
                     }
    temp <- cached co $ genSynth freq dur
    run $ k $ Audio co temp Nothing (Just 0)
run (Free (Source (Silence dur) k)) = do
    let co = CObject { coOp = "Silence (" ++ show dur ++ ")"
                     , coDeps = []
                     }
    temp <- cached co $ genSilence dur
    run $ k $ Audio co temp Nothing Nothing
run (Free (Bind op k)) = do
    let newBPM = opBPM op
        newStart = opStart op
        co = opCo op
    temp <- cached co $ interpretOp op
    run $ k $ Audio co temp newBPM newStart

-- | A pure version of 'run': just return the steps that will be done.
steps :: Prog Audio -> [String]
steps (Pure _) = []
steps (Free (Source (File tr) k)) = ("decode " ++ show (trackFormat tr)) : steps (k noAudio)
steps (Free (Source (Synth freq dur) k)) = ("synth " ++ show freq ++ " " ++ show dur) : steps (k noAudio)
steps (Free (Source (Silence dur) k)) = ("silence " ++ show dur) : steps (k noAudio)
steps (Free (Bind (OpSoxFX fx _) k)) = ("soxfx " ++ head (soxCompile fx)) : steps (k noAudio)
steps (Free (Bind (Merge _ _) k)) = "merge" : steps (k noAudio)
steps (Free (Bind (Sequence _ _) k)) = "sequence" : steps (k noAudio)

noAudio :: Audio
noAudio = undefined
