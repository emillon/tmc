import Control.Monad
import Control.Monad.Free
import System.Cmd

decodeMp3 :: FilePath -> FilePath -> IO ()
decodeMp3 input output =
    void $ system $ "lame --decode " ++ input ++ " " ++ output

decodeFlac :: FilePath -> FilePath -> IO ()
decodeFlac input output =
    void $ system $ "flac -f --decode " ++ input ++ " -o " ++ output

applySoxFx :: FilePath -> FilePath -> String -> IO ()
applySoxFx input output fx = do
    void $ system $ "sox " ++ input ++ " " ++ output ++ " " ++ fx

warpFile :: FilePath -> Double -> FilePath -> IO ()
warpFile input ratio output = do
    applySoxFx input output $ "speed " ++ show ratio

shiftFile :: FilePath -> Double -> FilePath -> IO ()
shiftFile input amount output = do
    applySoxFx input output $ "pad " ++ show amount

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles a b output =
    void $ system $ "sox -m " ++ a ++ " " ++ b ++ " " ++ output

gainFile :: FilePath -> Double -> FilePath -> IO ()
gainFile input amount output =
    applySoxFx input output $ "gain " ++ show amount

warpRatio :: Double
warpRatio = newBPM / oldBPM
    where
        newBPM = instruBPM
        oldBPM = acapBPM
        acapBPM = 125
        instruBPM = 133

shiftAmount :: Double
shiftAmount = 5.899

gainAmount :: Double
gainAmount = -3

main0 :: IO ()
main0 = do
    let acapFile = "katy.wav"
        instruFile = "walkonby.wav"
        warpedAcapFile = "katyW.wav"
        shiftedAcapFile = "katyWS.wav"
        gainAcapFile = "katyWSG.wav"
        resultFile = "res.wav"
    decodeMp3 "katy.mp3" acapFile
    decodeFlac "walkonby.flac" instruFile
    warpFile acapFile warpRatio warpedAcapFile
    shiftFile warpedAcapFile shiftAmount shiftedAcapFile
    gainFile shiftedAcapFile gainAmount gainAcapFile
    mergeFiles gainAcapFile instruFile resultFile

data ProgF a = MP3File FilePath (Audio -> a)
             | FlacFile FilePath (Audio -> a)
             | SoxFX String a
             | Merge a a

instance Functor ProgF where
    fmap f (MP3File path k) = MP3File path (f . k)
    fmap f (FlacFile path k) = FlacFile path (f . k)
    fmap f (SoxFX fx k) = SoxFX fx (f k)
    fmap f (Merge a b) = Merge (f a) (f b)

type Prog a = Free ProgF a

data Audio = Audio FilePath

flacFile :: FilePath -> Prog Audio
flacFile fp = liftF $ FlacFile fp id

mp3File :: FilePath -> Prog Audio
mp3File fp = liftF $ MP3File fp id

warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = liftF . SoxFX ("speed " ++ show ratio)

shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = liftF . SoxFX ("pad " ++ show amount)

gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = liftF . SoxFX ("gain " ++ show amount)

mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Merge a b

p :: Prog Audio
p = do
    acap <- mp3File "katy.mp3"
    instr <- flacFile "walkonby.flac"
    warpedAcap <- warpAudio warpRatio acap
    shiftedAcap <- shiftAudio shiftAmount warpedAcap
    gainAcap <- gainAudio gainAmount shiftedAcap
    mergeAudio instr gainAcap

run :: Prog Audio -> IO Audio
run (Pure x) = return x
run (Free (MP3File path k)) = do
    let temp = "decodemp3." ++ path ++ ".wav"
    decodeMp3 path temp
    run $ k $ Audio temp
run (Free (FlacFile path k)) = do
    let temp = "decodeflac." ++ path ++ ".wav"
    decodeFlac path temp
    run $ k $ Audio temp
run (Free (SoxFX fx k)) = do
    Audio input <- run k
    let temp = "fx" ++ fx ++ "." ++ input
    applySoxFx input temp fx
    return $ Audio temp
run (Free (Merge ka kb)) = do
    Audio a <- run ka
    Audio b <- run kb
    let temp = a ++ "merged" ++ b
    mergeFiles a b temp
    return $ Audio temp

steps :: Prog Audio -> [String]
steps (Pure _) = undefined
steps (Free (MP3File _ k)) = "decode mp3" : steps (k (Audio undefined))
steps (Free (FlacFile _ k)) = "decode flac" : steps (k (Audio undefined))
steps (Free (SoxFX fx r)) = ("sox fx " ++ fx) : steps r
steps (Free (Merge _ _)) = ["merge"]

main :: IO ()
main = void $ run p
