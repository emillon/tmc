-- | An example bootleg.
module Music.TMC.Example
    ( -- * Tracks
      katy
    , walkOnBy
      -- * Bootleg
    , exampleBootleg
      -- * Go!
    , main
    ) where

import Music.TMC.Prog
import Music.TMC.Tools
import Music.TMC.Types

-- | Katy Perry - Last Friday Night acapella.
katy :: Track
katy =
    Track { trackFormat = Mp3
          , trackPath = "katy.mp3"
          , trackBPM = BPM 125
          , trackStart = Duration 1.365
          }

-- | Miss Kittin & The Hacker - Walk On By.
walkOnBy :: Track
walkOnBy =
    Track { trackFormat = Flac
          , trackPath = "walkonby.flac"
          , trackBPM = BPM 133.756
          , trackStart = Duration 0.023
          }

-- | The bootleg itself.
exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- audioTrack katy
    instr <- audioTrack walkOnBy
    warpedAcap <- warpTo instr acap
    alignedAcap <- alignTo instr warpedAcap
    shiftedAcap <- shiftBeats 16 alignedAcap
    gainAcap <- gainAudio (Gain (-3)) shiftedAcap
    mergeAudio instr gainAcap

-- | Compile the bootleg.
main :: IO ()
main = tmcMain exampleBootleg
