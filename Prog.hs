{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The DSL itself.

module Prog ( -- * Be safe kids, use newtypes
              BPM(..)
            , bpmRatio
            , beatLen
            , Duration(..)
            , durationAdd
            , durationDiff
            , durationTimes
            , Frequency(..)
            , Gain(..)
              -- * Audio Tracks
            , Audio
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
            , warpTo
            , alignTo
              -- * Interpreter
            , run
            , steps
            ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Maybe
import System.Cmd
import Text.Printf

import Cache

-- | Beats Per Minute.
newtype BPM = BPM Double
    deriving (Show)

-- | Ratio of two BPMs.
bpmRatio :: BPM -> BPM -> Double
bpmRatio (BPM a) (BPM b) =
    a / b

-- | Duration of a beat.
beatLen :: BPM -> Duration
beatLen (BPM bpm) =
    Duration $ 60 / bpm

-- | A duration, in seconds.
newtype Duration = Duration Double
    deriving (Show)

-- | Add two Durations.
durationAdd :: Duration -> Duration -> Duration
durationAdd (Duration a) (Duration b) =
    Duration $ a + b

-- | Difference between two Durations.
durationDiff :: Duration -> Duration -> Duration
durationDiff (Duration a) (Duration b) =
    Duration $ a - b

-- | Multiply an integer and a Duration.
durationTimes :: Int -> Duration -> Duration
durationTimes n (Duration a) =
    Duration $ fromIntegral n * a
-- | A frequency, in Hertz.
newtype Frequency = Frequency Double
    deriving (Show)

-- | A gain, in DB.
newtype Gain = Gain Double

-- | Type of an audio source
data AudioType = Mp3
               | Flac
    deriving (Show)

-- | A bounced audio track.
-- This corresponds to an intermediate WAV file with
-- metadata such as BPM or start time.
data Audio = Audio { aCache :: CObject
                   , aPath :: FilePath
                   , aBPM :: Maybe BPM -- ^ Retrieve the BPM of an audio track
                   , aStart :: Maybe Duration -- ^ Retrieve the start time of an audio track
                   }
    deriving (Show)

data ProgF a = Bind Op (Audio -> a)

data Op = File Track
        | Synth Frequency Duration
        | Silence Duration
        | OpSoxFX SoxFX Audio
        | Merge Audio Audio
        | Sequence Audio Audio

instance Functor ProgF where
    fmap f (Bind op k) = Bind op (f . k)

-- | Our main Monad.
newtype Prog a = Prog (Free ProgF a)
    deriving (Monad)

data SoxFX = SoxTempo Double
           | SoxPad Duration
           | SoxGain Gain

soxCompile :: SoxFX -> [String]
soxCompile (SoxTempo ratio) = ["tempo", showD ratio]
soxCompile (SoxPad (Duration amount)) = ["pad", showD amount]
soxCompile (SoxGain (Gain amount)) = ["gain", showD amount]

showD :: Double -> String
showD d = printf "%f" d

soxFX :: SoxFX -> Audio -> Prog Audio
soxFX fx a = Prog $ liftF $ Bind (OpSoxFX fx a) id

-- | Change the speed of a track without altering its pitch
warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ SoxTempo ratio

-- | Pad with silence before the start.
shiftAudio :: Duration -> Audio -> Prog Audio
shiftAudio amount = soxFX $ SoxPad amount

-- | Adjust the gain (in DBs).
gainAudio :: Gain -> Audio -> Prog Audio
gainAudio amount = soxFX $ SoxGain amount

-- | Mix tracks together.
mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = Prog $ liftF $ Bind (Merge a b) id

-- | An audio file on disk. 'trackStart' is the position of beat zero.
data Track = Track { trackFormat :: AudioType
                   , trackPath :: FilePath
                   , trackBPM :: BPM
                   , trackStart :: Duration
                   }

-- | Read a 'Track'.
audioTrack :: Track -> Prog Audio
audioTrack t = Prog $ liftF $ (Bind $ File t) id

-- | Generate a sine wave.
synth :: Frequency -> Duration -> Prog Audio
synth freq dur = Prog $ liftF $ (Bind $ Synth freq dur) id

-- | Generate silence.
silence :: Duration -> Prog Audio
silence dur = Prog $ liftF $ (Bind$ Silence dur) id

-- | Join tracks (play one after another).
sequenceAudio :: Audio -> Audio -> Prog Audio
sequenceAudio a b = Prog $ liftF $ Bind (Sequence a b) id

-- | An extension of 'sequenceAudio' to lists.
seqList :: [Audio] -> Prog Audio
seqList [] = error "seqList"
seqList [a] = return a
seqList (a:as) = do
    r <- seqList as
    sequenceAudio a r

opBPM :: Op -> Maybe BPM
opBPM (File track) = Just $ trackBPM track
opBPM (Synth _ _) = Nothing
opBPM (Silence _) = Nothing
opBPM (OpSoxFX sfx a) = soxBPM sfx <$> aBPM a
opBPM (Merge a _b) = aBPM a -- we assume that we're mixing similar tracks
opBPM (Sequence _ _) = Nothing -- could optimize when a & b are close

soxBPM :: SoxFX -> BPM -> BPM
soxBPM (SoxTempo ratio) (BPM x) = BPM $ ratio * x
soxBPM (SoxPad _) x = x
soxBPM (SoxGain _) x = x

opStart :: Op -> Maybe Duration
opStart (File track) = Just $ trackStart track
opStart (Synth _ _) = Just $ Duration 0
opStart (Silence _) = Nothing
opStart (OpSoxFX sfx a) = soxStart sfx <$> aStart a
opStart (Merge a _b) = aStart a -- we assume that we're mixing aligned tracks
opStart (Sequence a _b) = aStart a


soxStart :: SoxFX -> Duration -> Duration
soxStart (SoxTempo ratio) (Duration x) = Duration $ ratio * x
soxStart (SoxPad (Duration shift)) (Duration x) = Duration $ shift + x
soxStart (SoxGain _) x = x

coMake :: String -> [Audio] -> CObject
coMake op deps =
    CObject { coOp = op
            , coDeps = map (coHash . aCache) deps
            }

opCo :: Op -> CObject
opCo (File _) = coMake "File" []
opCo (Synth freq dur) = coMake ("Synth (" ++ show freq ++ ", " ++ show dur ++ ")") []
opCo (Silence dur) = coMake ("Silence (" ++ show dur ++ ")") []
opCo (OpSoxFX sfx a) = coMake ("SoxFX (" ++ unwords (soxCompile sfx) ++ ")") [a]
opCo (Merge a b) = coMake "Merge" [a, b]
opCo (Sequence a b) = coMake "Sequence" [a, b]

-- | A track that only does beep beep.
-- There are actually two different beeps, a high one and a low one.
-- It does H L L L H L L L... (4/4)
metronome :: BPM
          -> Int -- ^ Number of bars
          -> Prog Audio
metronome bpm nbars = do
    hiBeep <- synth hiFreq beepLen
    sil <- silence silenceLen
    loBeep <- synth loFreq beepLen
    bar <- seqList [hiBeep, sil, loBeep, sil, loBeep, sil, loBeep, sil]
    seqList $ replicate nbars bar
        where
            beepLen = Duration 0.1
            hiFreq = Frequency $ 2 * baseFreq
            loFreq = Frequency baseFreq
            baseFreq = 440
            silenceLen = durationDiff (beatLen bpm) beepLen

-- | Play a metronome synchronized on top of a track.
-- It should be an easy way to know if the track's metadata is correct.
checkBPM :: Track -> Prog Audio
checkBPM track = do
    a <- audioTrack track
    m <- metronome (trackBPM track) 16
    s <- shiftAudio (trackStart track) m
    mergeAudio a s

-- | Change the tempo of a track (using 'warpAudio') so that it has the same
-- speed as a destination track.
warpTo :: Audio -- ^ Destination
       -> Audio -- ^ Source
       -> Prog Audio
warpTo dest src =
    warpAudio ratio src
        where
            ratio = fromMaybe (error "warpTo: no BPM on track") $ do
                destBPM <- aBPM dest
                origBPM <- aBPM src
                return $ bpmRatio destBPM origBPM

-- | Add padding at the beginning of a track so that the first beat coincides
-- with that of another track.
alignTo :: Audio -- ^ Destination
        -> Audio -- ^ Source
        -> Int -- ^ Beat offset
        -> Prog Audio
alignTo dest src beatOff =
    shiftAudio shiftAmount src
        where
            shiftAmount = fromMaybe (error "alignTo: missing a start time or BPM") $ do
                bpm <- aBPM dest
                destStart <- aStart dest
                srcStart <- aStart src
                return $ durationAdd (durationDiff destStart srcStart) (durationTimes beatOff (beatLen bpm))

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
interpretOp (File (Track { trackFormat = fmt, trackPath = path })) temp =
    decodeFile fmt path temp
interpretOp (Synth freq dur) temp =
    genSynth freq dur temp
interpretOp (Silence dur) temp =
    genSilence dur temp

decodeFile :: AudioType -> FilePath -> FilePath -> IO ()
decodeFile Mp3 input output =
    execCommand "lame" ["--decode", input, output]
decodeFile Flac input output =
    execCommand "flac" ["-f", "--decode", "--no-preserve-modtime", input, "-o", output]

genSynth :: Frequency -> Duration -> FilePath -> IO ()
genSynth (Frequency freq) (Duration dur) output =
    execCommand "sox" ["-n", "-r", "44100", output, "synth", showD dur, "sine", showD freq]

genSilence :: Duration -> FilePath -> IO ()
genSilence (Duration dur) output =
    execCommand "sox" ["-n", "-r", "44100", output, "trim", "0", showD dur]

-- | Execute the program: invoke tools that actually do the manipulation.
run :: Prog Audio -> IO Audio
run (Prog (Pure x)) = return x
run (Prog (Free (Bind op k))) = do
    let newBPM = opBPM op
        newStart = opStart op
        co = opCo op
    temp <- cached co $ interpretOp op
    run $ Prog $ k $ Audio co temp newBPM newStart

-- | A pure version of 'run': just return the steps that will be done.
steps :: Prog Audio -> [String]
steps (Prog (Pure _)) = []
steps (Prog (Free (Bind (File tr) k))) = ("decode " ++ show (trackFormat tr)) : steps (Prog (k noAudio))
steps (Prog (Free (Bind (Synth freq dur) k))) = ("synth " ++ show freq ++ " " ++ show dur) : steps (Prog (k noAudio))
steps (Prog (Free (Bind (Silence dur) k))) = ("silence " ++ show dur) : steps (Prog (k noAudio))
steps (Prog (Free (Bind (OpSoxFX fx _) k))) = ("soxfx " ++ head (soxCompile fx)) : steps (Prog (k noAudio))
steps (Prog (Free (Bind (Merge _ _) k))) = "merge" : steps (Prog (k noAudio))
steps (Prog (Free (Bind (Sequence _ _) k))) = "sequence" : steps (Prog (k noAudio))

noAudio :: Audio
noAudio = undefined
