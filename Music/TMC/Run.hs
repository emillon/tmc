-- | The TMC interpreter.

module Music.TMC.Run
    ( run
    , steps
    )
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import System.Process
import Text.Printf

import Music.TMC.Cache
import Music.TMC.Internals
import Music.TMC.Types

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
soxBPM (SoxTrim _ _) x = x

opStart :: Op -> Maybe Duration
opStart (File track) = Just $ trackStart track
opStart (Synth _ _) = Just $ Duration 0
opStart (Silence _) = Nothing
opStart (OpSoxFX sfx a) = do
    sa <- aStart a
    soxStart sfx sa
opStart (Merge a _b) = aStart a -- we assume that we're mixing aligned tracks
opStart (Sequence a _b) = aStart a

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
opDescr (Sequence _ _) = "Sequence"

opDeps :: Op -> [Audio]
opDeps (File _) = []
opDeps (Synth _ _) = []
opDeps (Silence _) = []
opDeps (OpSoxFX _ a) = [a]
opDeps (Merge a b) = [a, b]
opDeps (Sequence a b) = [a, b]

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
showShortOp (Sequence _ _) = "sequence"

execCommand :: String -> [String] -> IO ()
execCommand cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args) { std_out = CreatePipe, std_err = CreatePipe }
    void $ waitForProcess p

interpretOp :: Op -> FilePath -> IO ()
interpretOp (OpSoxFX fx (Audio _ input _ _)) temp =
    execCommand "sox" $ [input, temp] ++ soxCompile fx
interpretOp (Merge a b) temp =
    execCommand "sox" ["-m", aPath a, aPath b, temp]
interpretOp (Sequence a b) temp =
    execCommand "sox" [aPath a, aPath b, temp]
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

noAudio :: Audio
noAudio = undefined

-- | Execute the program: invoke tools that actually do the manipulation.
run :: Prog Audio -> IO Audio
run (Prog (Pure x)) = return x
run (Prog (Free (Bind op k))) = do
    let newBPM = opBPM op
        newStart = opStart op
        co = makeCo (opDescr op) (map aCache $ opDeps op) (interpretOp op)
    hit <- isCached co
    let cachedMsg = if hit then "[ HIT ]" else "[ EXP ]"
        msg = cachedMsg ++ " Running op: " ++ showShortOp op
    putStrLn msg
    temp <- cached co
    run $ Prog $ k $ Audio co temp newBPM newStart

-- | A pure version of 'run': just return the steps that will be done.
steps :: Prog Audio -> [String]
steps (Prog (Pure _)) = []
steps (Prog (Free (Bind op k))) = showShortOp op : steps (Prog (k noAudio))
