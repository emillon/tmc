module Example (exampleBootleg) where

import Data.Maybe

import Prog

katy :: Track
katy =
    Track { trackFormat = Mp3
          , trackPath = "katy.mp3"
          , trackBPM = 125
          , trackStart = 1.365
          }

walkOnBy :: Track
walkOnBy =
    Track { trackFormat = Flac
          , trackPath = "walkonby.flac"
          , trackBPM = 133.756
          , trackStart = 0.023
          }

warpTo :: Audio -> Audio -> Prog Audio
warpTo dest src =
    warpAudio ratio src
        where
            ratio = fromMaybe (error "warpTo: no BPM on track") $ do
                destBPM <- aBPM dest
                origBPM <- aBPM src
                return $ destBPM / origBPM

alignTo :: Audio -> Audio -> Int -> Prog Audio
alignTo dest src beatOff =
    shiftAudio shiftAmount src
        where
            shiftAmount = fromMaybe (error "alignTo: missing a start time or BPM") $ do
                bpm <- aBPM dest
                destStart <- aStart dest
                srcStart <- aStart src
                return $ destStart - srcStart + fromIntegral beatOff * 60 / bpm

exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- audioTrack katy
    instr <- audioTrack walkOnBy
    warpedAcap <- warpTo instr acap
    shiftedAcap <- alignTo instr warpedAcap 16
    gainAcap <- gainAudio (-3) shiftedAcap
    mergeAudio instr gainAcap
