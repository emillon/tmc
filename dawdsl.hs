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

data ProgF a = Bind Op (Audio -> a)

data Op = MP3File FilePath
        | FlacFile FilePath
        | SoxFX String Audio
        | Merge Audio Audio

instance Functor ProgF where
    fmap f (Bind op k) = Bind op (f . k)

type Prog a = Free ProgF a

data Audio = Audio FilePath

flacFile :: FilePath -> Prog Audio
flacFile fp = liftF $ Bind (FlacFile fp) id

mp3File :: FilePath -> Prog Audio
mp3File fp = liftF $ Bind (MP3File fp) id

soxFX :: String -> Audio -> Prog Audio
soxFX fx a = liftF $ Bind (SoxFX fx a) id

warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ "tempo " ++ show ratio

shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = soxFX $ "pad " ++ show amount

gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = soxFX $ "gain " ++ show amount

mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Bind (Merge a b) id

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

interpretOp :: Op -> FilePath -> IO ()
interpretOp (MP3File path) temp = decodeMp3 path temp
interpretOp (FlacFile path) temp = decodeFlac path temp
interpretOp (SoxFX fx (Audio input)) temp = applySoxFx input temp fx
interpretOp (Merge (Audio a) (Audio b)) temp = mergeFiles a b temp

run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (Bind op k)) = do
    temp <- nextTemp
    interpretOp op temp
    run $ k $ Audio temp

steps :: Prog Audio -> [String]
steps (Pure _) = []
steps (Free (Bind (MP3File _) k)) = "decode mp3" : steps (k (Audio undefined))
steps (Free (Bind (FlacFile _) k)) = "decode flac" : steps (k (Audio undefined))
steps (Free (Bind (SoxFX fx _) k)) = ("soxfx " ++ fx) : steps (k (Audio undefined))
steps (Free (Bind (Merge _ _) k)) = "merge" : steps (k (Audio undefined))

main :: IO ()
main = do
    Audio f <- run p
    print f
