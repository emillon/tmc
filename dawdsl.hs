import Control.Monad
import Control.Monad.Free
import System.Cmd
import System.Directory

execCommand :: String -> [String] -> IO ()
execCommand cmd args =
    void $ rawSystem cmd args

decodeMp3 :: FilePath -> FilePath -> IO ()
decodeMp3 input output =
    execCommand "lame" ["--decode", input, output]

decodeFlac :: FilePath -> FilePath -> IO ()
decodeFlac input output =
    execCommand "flac" ["-f", "--decode", input, "-o", output]

applySoxFx :: FilePath -> FilePath -> String -> IO ()
applySoxFx input output fx =
    execCommand "sox" $ [input, output] ++ words fx

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles a b output =
    execCommand "sox" ["-m", a, b, output]

data ProgF a = MP3File FilePath (Audio -> a)
             | FlacFile FilePath (Audio -> a)
             | SoxFX String Audio (Audio -> a)
             | Merge Audio Audio (Audio -> a)

instance Functor ProgF where
    fmap f (MP3File path k) = MP3File path (f . k)
    fmap f (FlacFile path k) = FlacFile path (f . k)
    fmap f (SoxFX fx a k) = SoxFX fx a (f . k)
    fmap f (Merge a b k) = Merge a b (f . k)

type Prog a = Free ProgF a

data Audio = Audio FilePath

flacFile :: FilePath -> Prog Audio
flacFile fp = liftF $ FlacFile fp id

mp3File :: FilePath -> Prog Audio
mp3File fp = liftF $ MP3File fp id

soxFX :: String -> Audio -> Prog Audio
soxFX fx a = liftF $ SoxFX fx a id

warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ "tempo " ++ show ratio

shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = soxFX $ "pad " ++ show amount

gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = soxFX $ "gain " ++ show amount

mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Merge a b id

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

p :: Prog Audio
p = do
    acap <- mp3File "katy.mp3"
    instr <- flacFile "walkonby.flac"
    warpedAcap <- warpAudio (133 / 125) acap
    shiftedAcap <- shiftAudio 5.899 warpedAcap
    gainAcap <- gainAudio (-3) shiftedAcap
    mergeAudio instr gainAcap

run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (MP3File path k)) = do
    temp <- nextTemp
    decodeMp3 path temp
    run $ k $ Audio temp
run (Free (FlacFile path k)) = do
    temp <- nextTemp
    decodeFlac path temp
    run $ k $ Audio temp
run (Free (SoxFX fx (Audio input) k)) = do
    temp <- nextTemp
    applySoxFx input temp fx
    run $ k $ Audio temp
run (Free (Merge (Audio a) (Audio b) k)) = do
    temp <- nextTemp
    mergeFiles a b temp
    run $ k $ Audio temp

steps :: Prog Audio -> [String]
steps (Pure _) = []
steps (Free (MP3File _ k)) = "decode mp3" : steps (k (Audio undefined))
steps (Free (FlacFile _ k)) = "decode flac" : steps (k (Audio undefined))
steps (Free (SoxFX fx _ k)) = ("soxfx " ++ fx) : steps (k (Audio undefined))
steps (Free (Merge _ _ k)) = "merge" : steps (k (Audio undefined))

main :: IO ()
main = void $ run p
