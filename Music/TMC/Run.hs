{-# LANGUAGE FlexibleInstances #-}

-- | The TMC interpreter.

module Music.TMC.Run
    ( run
    , runVerbose
    , runWith
    , RunOptions(..)
    , steps
    , audioMeta
    )
    where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Data.Default
import System.Exit
import System.Process
import Text.Printf

import Music.TMC.Cache
import Music.TMC.Internals
import Music.TMC.Logger
import Music.TMC.Optimize
import Music.TMC.Prog
import Music.TMC.Types

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

execCommand :: Bool -> String -> [String] -> IO ()
execCommand verb cmd args = do
    let h = if verb then Inherit else CreatePipe
    (_, _, _, p) <- createProcess (proc cmd args) { std_out = h, std_err = h }
    exitCode <- waitForProcess p
    unless (exitCode == ExitSuccess) $ error $ "Command failed: " ++ unwords (cmd:args)

interpretOp :: Bool -> Op -> FilePath -> IO ()
interpretOp verb (OpSoxFX fx a) temp =
    execCommand verb "sox" $ [aPath a, temp] ++ soxCompile fx
interpretOp verb (Merge a b) temp =
    execCommand verb "sox" ["-m", aPath a, aPath b, temp]
interpretOp verb (Sequence l) temp =
    execCommand verb "sox" $ map aPath l ++ [temp]
interpretOp verb (File (Track { trackFormat = fmt, trackPath = path })) temp =
    decodeFile verb fmt path temp
interpretOp verb (Synth (Frequency freq) (Duration dur)) temp =
    execCommand verb "sox" ["-n", "-r", "44100", temp, "synth", showD dur, "sine", showD freq]
interpretOp verb (Silence (Duration dur)) temp =
    execCommand verb "sox" ["-n", "-r", "44100", temp, "trim", "0", showD dur]

decodeFile :: Bool -> AudioType -> FilePath -> FilePath -> IO ()
decodeFile verb Mp3 input output =
    execCommand verb "lame" ["--decode", input, output]
decodeFile verb Flac input output =
    execCommand verb "flac" ["-f", "--decode", "--no-preserve-modtime", input, "-o", output]

-- | Build a 'Audio' object out of an 'Op'.
-- It can be used both in pure and impure code, even if the associated file does
-- not exist.
audioMeta :: Op -> Bool -> Audio
audioMeta op showOutput =
    Audio
        { aCache = makeCo (opDescr op) (map aCache $ opDeps op) (interpretOp showOutput op)
        , aBPM = opBPM op
        , aStart = opStart op
        }

-- | How to run a program.
data RunOptions = RunOptions
    { optLogLevel :: LogLevel
    , optOptimize :: Bool
    }

type Exec a = ReaderT RunOptions IO a

instance Monad m => MonadLogger (ReaderT RunOptions m)  where
    getLogLevel = asks optLogLevel

instance Default RunOptions where
    def = RunOptions
            { optLogLevel = LogNotice
            , optOptimize = False
            }

-- | Execute the program: invoke tools that actually do the manipulation.
run :: Prog Audio -> IO Audio
run = runWith def

-- | Execute the program in verbose mode.
runVerbose :: Prog Audio -> IO Audio
runVerbose =
    runWith opts
        where
            opts = def { optLogLevel = LogInfo }

-- | A variant of 'run' when you can pass options.
runWith :: RunOptions -> Prog Audio -> IO Audio
runWith opts baseProg =
    runReaderT (runWithM prog) opts
        where
            prog = maybeOpt (optOptimize opts) baseProg
            maybeOpt True = optimize
            maybeOpt False = id

runWithM :: Prog a -> Exec a
runWithM (Prog (Pure x)) = return x
runWithM (Prog (Free (Bind op k))) = do
    showOutput <- isLoggerActive LogDebug
    let a = audioMeta op showOutput
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
steps (Prog (Free (Bind op k))) = showShortOp op : steps (Prog (k (audioMeta op False)))
