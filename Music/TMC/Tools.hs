-- | Higher-level tools defined on programs.

module Music.TMC.Tools
    ( play
    , warpTo
    , alignTo
    , shiftBeats
    , metronome
    , checkBPM
    , tmcMain
    ) where

import Control.Monad
import Data.Default
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Process

import Music.TMC.Logger
import Music.TMC.Prog
import Music.TMC.Run
import Music.TMC.Types

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
        -> Prog Audio
alignTo dest src =
    shiftAudio shiftAmount src
        where
            shiftAmount = fromMaybe (error "alignTo: missing a start time or BPM") $ do
                destStart <- aStart dest
                srcStart <- aStart src
                return $ durationDiff destStart srcStart

-- | Shift a track N beats.
shiftBeats :: Int -> Audio -> Prog Audio
shiftBeats nbeats a =
    shiftAudio shiftAmount a
        where
            bpm = fromMaybe (error "shiftBeats: need a BPM on audio track") (aBPM a)
            shiftAmount = durationTimes nbeats (beatLen bpm)

-- | Play audio.
play :: Audio -> IO ()
play a =
    void $ rawSystem "mpv" [aPath a]

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
    replicateAudio nbars bar
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

data Flag = FlagVerbose
          | FlagOptimize

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["verbose"] (NoArg FlagVerbose) "be more verbose"
    , Option ['O'] ["optimize"] (NoArg FlagOptimize) "optimize the program first"
    ]

parseArgs :: [String] -> IO [Flag]
parseArgs argv =
    case getOpt Permute options argv of
        (o, [], []) -> return o
        (_, _, []) -> ioError $ userError "I don't no what to do with these non-options"
        (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options
     where header = "Usage: ic [OPTION...] files..."

interpretFlag :: Flag -> RunOptions -> RunOptions
interpretFlag FlagVerbose ro = ro { optLogLevel = moreVerbose $ optLogLevel ro }
interpretFlag FlagOptimize ro = ro { optOptimize = True }

-- | A convenient 'main' function that parses arguments on the command line.
tmcMain :: Prog Audio -> IO ()
tmcMain p = do
    argv <- getArgs
    flags <- parseArgs argv
    let opts = foldr interpretFlag def flags
    void $ runWith opts p
