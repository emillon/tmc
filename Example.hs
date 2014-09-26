module Example (exampleBootleg) where

import Prog

exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- sourceFile Mp3 "katy.mp3"
    instr <- sourceFile Flac "walkonby.flac"
    warpedAcap <- warpAudio (133 / 125) acap
    shiftedAcap <- shiftAudio 5.899 warpedAcap
    gainAcap <- gainAudio (-3) shiftedAcap
    mergeAudio instr gainAcap
