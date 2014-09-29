module Example (exampleBootleg) where

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
          , trackBPM = 133
          , trackStart = 0.023
          }

warpTo :: Audio -> Audio -> Prog Audio
warpTo dest src =
    warpAudio ratio src
        where
            ratioM = do
                destBPM <- aBPM dest
                origBPM <- aBPM src
                return $ destBPM / origBPM
            ratio = case ratioM of
                Nothing -> error "warpTo: no BPM on track"
                Just x -> x

exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- audioTrack katy
    instr <- audioTrack walkOnBy
    warpedAcap <- warpTo instr acap
    let shiftAmount = trackStart walkOnBy - trackStart katy + 16 * 60 / (trackBPM walkOnBy)
    shiftedAcap <- shiftAudio shiftAmount warpedAcap
    gainAcap <- gainAudio (-3) shiftedAcap
    mergeAudio instr gainAcap
