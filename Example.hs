module Example (exampleBootleg) where

import Prog

katy :: Track
katy =
    Track { trackFormat = Mp3
          , trackPath = "katy.mp3"
          , trackBPM = 125
          }

walkOnBy :: Track
walkOnBy =
    Track { trackFormat = Flac
          , trackPath = "walkonby.flac"
          , trackBPM = 133
          }

exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- audioTrack katy
    instr <- audioTrack walkOnBy
    warpedAcap <- warpAudio ((trackBPM walkOnBy) / (trackBPM katy)) acap
    shiftedAcap <- shiftAudio 5.899 warpedAcap
    gainAcap <- gainAudio (-3) shiftedAcap
    mergeAudio instr gainAcap
