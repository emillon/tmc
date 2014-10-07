{-# LANGUAGE FlexibleInstances #-}

-- | The TMC interpreter.

module Music.TMC.Run
    ( run
    , runVerbose
    , runWith
    , steps
    , audioMeta
    )
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Data.Default
import System.Process
import Text.Printf

import Music.TMC.Cache
import Music.TMC.Internals
import Music.TMC.Logger
import Music.TMC.Prog
import Music.TMC.Types

opBPM :: Op -> Maybe BPM
opBPM (File track) = Just $ trackBPM track
opBPM (Synth _ _) = Nothing
opBPM (Silence _) = Nothing
opBPM (OpSoxFX sfx a) = soxBPM sfx <$> aBPM a
opBPM (Merge a _b) = aBPM a -- we assume that we're mixing similar tracks
opBPM (Sequence _) = Nothing -- could optimize when all in the list are close

soxBPM :: SoxFX -> BPM -> BPM
soxBPM (SoxTempo ratio) (BPM x) = BPM $ ratio * x
soxBPM (SoxPad _) x = x
soxBPM (SoxGain _) x = x
soxBPM (SoxTrim _ _) x = x

opStart :: Op -> Maybe Duration
opStart (File track) = Just $ trackStart track
opStart (Synth _ _) = Just $ Duration 0
opStart (Silence _) = Nothing
opStart (OpSoxFX sfx a) = do
    sa <- aStart a
    soxStart sfx sa
opStart (Merge a _b) = aStart a -- we assume that we're mixing aligned tracks
opStart (Sequence _) = Nothing -- could optimize when all in the list are close

soxStart :: SoxFX -> Duration -> Maybe Duration
soxStart (SoxTempo ratio) (Duration x) = Just $ Duration $ ratio * x
soxStart (SoxPad shift) x = Just $ durationAdd shift x
soxStart (SoxGain _) x = Just x
soxStart (SoxTrim _ _) _ = Nothing -- maybe it's possible to compute it

opDescr :: Op -> String
opDescr (File a) = "File (" ++ show a ++ ")"
opDescr (Synth freq dur) = "Synth (" ++ show freq ++ ", " ++ show dur ++ ")"
opDescr (Silence dur) = "Silence (" ++ show dur ++ ")"
opDescr (OpSoxFX sfx _) = "SoxFX (" ++ unwords (soxCompile sfx) ++ ")"
opDescr (Merge _ _) = "Merge"
opDescr (Sequence _) = "Sequence"

opDeps :: Op -> [Audio]
opDeps (File _) = []
opDeps (Synth _ _) = []
opDeps (Silence _) = []
opDeps (OpSoxFX _ a) = [a]
opDeps (Merge a b) = [a, b]
opDeps (Sequence l) = l

soxCompile :: SoxFX -> [String]
soxCompile (SoxTempo ratio) = ["tempo", showD ratio]
soxCompile (SoxPad (Duration amount)) = ["pad", showD amount]
soxCompile (SoxGain (Gain amount)) = ["gain", showD amount]
soxCompile (SoxTrim (Duration start) (Duration end)) = ["trim", showD start, showD end]

showD :: Double -> String
showD d = printf "%f" d

showShortOp :: Op -> String
showShortOp (File tr) = "decode " ++ show (trackFormat tr)
showShortOp (Synth freq dur) = "synth " ++ show freq ++ " " ++ show dur
showShortOp (Silence dur) = "silence " ++ show dur
showShortOp (OpSoxFX fx _) = "soxfx " ++ head (soxCompile fx)
showShortOp (Merge _ _) = "merge"
showShortOp (Sequence _) = "sequence"

execCommand :: String -> [String] -> IO ()
execCommand cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args) { std_out = CreatePipe, std_err = CreatePipe }
    void $ waitForProcess p

interpretOp :: Op -> FilePath -> IO ()
interpretOp (OpSoxFX fx a) temp =
    execCommand "sox" $ [aPath a, temp] ++ soxCompile fx
interpretOp (Merge a b) temp =
    execCommand "sox" ["-m", aPath a, aPath b, temp]
interpretOp (Sequence l) temp =
    execCommand "sox" $ map aPath l ++ [temp]
interpretOp (File (Track { trackFormat = fmt, trackPath = path })) temp =
    decodeFile fmt path temp
interpretOp (Synth (Frequency freq) (Duration dur)) temp =
    execCommand "sox" ["-n", "-r", "44100", temp, "synth", showD dur, "sine", showD freq]
interpretOp (Silence (Duration dur)) temp =
    execCommand "sox" ["-n", "-r", "44100", temp, "trim", "0", showD dur]

decodeFile :: AudioType -> FilePath -> FilePath -> IO ()
decodeFile Mp3 input output =
    execCommand "lame" ["--decode", input, output]
decodeFile Flac input output =
    execCommand "flac" ["-f", "--decode", "--no-preserve-modtime", input, "-o", output]

-- | Build a 'Audio' object out of an 'Op'.
-- It can be used both in pure and impure code, even if the associated file does
-- not exist.
audioMeta :: Op -> Audio
audioMeta op =
    Audio
        { aCache = makeCo (opDescr op) (map aCache $ opDeps op) (interpretOp op)
        , aBPM = opBPM op
        , aStart = opStart op
        }

-- | How to run a program.
data RunOptions = RunOptions
    { logLevel :: LogLevel
    }

type Exec a = ReaderT RunOptions IO a

instance Monad m => MonadLogger (ReaderT RunOptions m)  where
    getLogLevel = asks logLevel

instance Default RunOptions where
    def = RunOptions
            { logLevel = LogNotice
            }

-- | Execute the program: invoke tools that actually do the manipulation.
run :: Prog Audio -> IO Audio
run = runWith def

-- | Execute the program in verbose mode.
runVerbose :: Prog Audio -> IO Audio
runVerbose =
    runWith opts
        where
            opts = def { logLevel = LogInfo }

-- | A variant of 'run' when you can pass options.
runWith :: RunOptions -> Prog Audio -> IO Audio
runWith opts prog =
    runReaderT (runWithM prog) opts

runWithM :: Prog a -> Exec a
runWithM (Prog (Pure x)) = return x
runWithM (Prog (Free (Bind op k))) = do
    let a = audioMeta op
        co = aCache a
    hit <- lift $ isCached co
    destMsg <- infoMsg $ " (" ++ shortHash co ++ ")"
    let cachedMsg = if hit then "[ HIT ]" else "[ EXP ]"
        msg = cachedMsg ++ destMsg ++ " Running op: " ++ showShortOp op
    noticeM msg
    _ <- lift $ cached co
    runWithM $ Prog $ k a

-- | A pure version of 'run': just return the steps that will be done.
steps :: Prog Audio -> [String]
steps (Prog (Pure _)) = []
steps (Prog (Free (Bind op k))) = showShortOp op : steps (Prog (k (audioMeta op)))
