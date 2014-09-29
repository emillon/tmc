module Run ( run
           , steps
           ) where

import Control.Monad
import Control.Monad.Free
import System.Cmd

import Cache
import Prog

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
    execCommand "sox" ["-n", output, "synth", show dur, "sine", show freq]

genSilence :: Double -> FilePath -> IO ()
genSilence dur output =
    execCommand "sox" [output, "trim", "0", show dur]

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
