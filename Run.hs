module Run ( run
           , steps
           ) where

import Control.Monad
import Control.Monad.Free
import System.Cmd
import System.Directory

import Prog

execCommand :: String -> [String] -> IO ()
execCommand cmd args =
    void $ rawSystem cmd args

nextTemp :: IO FilePath
nextTemp = go 0
    where
        go :: Integer -> IO FilePath
        go n = do
            let path = "/tmp/temp" ++ show n ++ ".wav"
            ex <- doesFileExist path
            if ex
                then go (n+1)
                else return path

interpretOp :: Op -> FilePath -> IO ()
interpretOp (OpSoxFX fx (Audio input _)) temp =
    execCommand "sox" $ [input, temp] ++ soxCompile fx
interpretOp (Merge (Audio a _) (Audio b _)) temp =
    execCommand "sox" ["-m", a, b, temp]

decodeFile :: AudioType -> FilePath -> FilePath -> IO ()
decodeFile Mp3 input output =
    execCommand "lame" ["--decode", input, output]
decodeFile Flac input output =
    execCommand "flac" ["-f", "--decode", input, "-o", output]

run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (File track k)) = do
    temp <- nextTemp
    let fmt = trackFormat track
        path = trackPath track
        bpm = trackBPM track
    decodeFile fmt path temp
    run $ k $ Audio temp (Just bpm)
run (Free (Bind op k)) = do
    temp <- nextTemp
    interpretOp op temp
    let newBPM = opBPM op
    run $ k $ Audio temp newBPM

steps :: Prog Audio -> [String]
steps (Pure _) = []
steps (Free (File tr k)) = ("decode " ++ show (trackFormat tr)) : steps (k noAudio)
steps (Free (Bind (OpSoxFX fx _) k)) = ("soxfx " ++ head (soxCompile fx)) : steps (k noAudio)
steps (Free (Bind (Merge _ _) k)) = "merge" : steps (k noAudio)

noAudio :: Audio
noAudio = undefined
