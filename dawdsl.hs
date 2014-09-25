import Control.Monad
import System.Cmd
import System.Directory

decodeMp3 :: FilePath -> FilePath -> IO ()
decodeMp3 input output =
    void $ system $ "lame --decode " ++ input ++ " " ++ output

decodeFlac :: FilePath -> FilePath -> IO ()
decodeFlac input output =
    void $ system $ "flac -f --decode " ++ input ++ " -o " ++ output

warpFile :: FilePath -> Double -> FilePath -> IO ()
warpFile input ratio output = do
    void $ system $ "sox " ++ input ++ " " ++ output ++ " speed " ++ show ratio

shiftFile :: FilePath -> Double -> FilePath -> IO ()
shiftFile input amount output = do
    void $ system $ "sox " ++ input ++ " " ++ output ++ " pad " ++ show amount

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles a b output =
    void $ system $ "sox -m " ++ a ++ " " ++ b ++ " " ++ output

gainFile :: FilePath -> Double -> FilePath -> IO ()
gainFile input amount output =
    void $ system $ "sox " ++ input ++ " " ++ output ++ " gain " ++ show amount

main :: IO ()
main = do
    let acapFile = "katy.wav"
        acapBPM = 125
        instruFile = "walkonby.wav"
        instruBPM = 133
        warpedAcapFile = "katyW.wav"
        newBPM = instruBPM
        oldBPM = acapBPM
        warpRatio = newBPM / oldBPM
        shiftedAcapFile = "katyWS.wav"
        shiftAmount = 5.899
        gainAcapFile = "katyWSG.wav"
        gainAmount = -3
        resultFile = "res.wav"
    decodeMp3 "katy.mp3" acapFile
    decodeFlac "walkonby.flac" instruFile
    warpFile acapFile warpRatio warpedAcapFile
    shiftFile warpedAcapFile shiftAmount shiftedAcapFile
    gainFile shiftedAcapFile gainAmount gainAcapFile
    mergeFiles gainAcapFile instruFile resultFile
