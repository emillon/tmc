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
            let path = "temp" ++ show n ++ ".wav"
            ex <- doesFileExist path
            if ex
                then go (n+1)
                else return path

interpretOp :: Op -> FilePath -> IO ()
interpretOp (SoxFX fx (Audio input)) temp =
    execCommand "sox" $ [input, temp] ++ words fx
interpretOp (Merge (Audio a) (Audio b)) temp =
    execCommand "sox" ["-m", a, b, temp]

decodeFile :: AudioType -> FilePath -> FilePath -> IO ()
decodeFile Mp3 input output =
    execCommand "lame" ["--decode", input, output]
decodeFile Flac input output =
    execCommand "flac" ["-f", "--decode", input, "-o", output]

run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (File typ path k)) = do
    temp <- nextTemp
    decodeFile typ path temp
    run $ k $ Audio temp
run (Free (Bind op k)) = do
    temp <- nextTemp
    interpretOp op temp
    run $ k $ Audio temp

steps :: Prog Audio -> [String]
steps (Pure _) = []
steps (Free (File typ _ k)) = ("decode " ++ show typ) : steps (k noAudio)
steps (Free (Bind (SoxFX fx _) k)) = ("soxfx " ++ fx) : steps (k noAudio)
steps (Free (Bind (Merge _ _) k)) = "merge" : steps (k noAudio)

noAudio :: Audio
noAudio = undefined
