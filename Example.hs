-- | An example bootleg.
module Example ( -- * Tracks
                 katy
               , walkOnBy
                 -- * Bootleg
               , exampleBootleg
               ) where

import Data.Maybe

import Prog

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

warpTo :: Audio -> Audio -> Prog Audio
warpTo dest src =
    warpAudio ratio src
        where
            ratio = fromMaybe (error "warpTo: no BPM on track") $ do
                destBPM <- aBPM dest
                origBPM <- aBPM src
                return $ bpmRatio destBPM origBPM

alignTo :: Audio -> Audio -> Int -> Prog Audio
alignTo dest src beatOff =
    shiftAudio shiftAmount src
        where
            shiftAmount = fromMaybe (error "alignTo: missing a start time or BPM") $ do
                bpm <- aBPM dest
                destStart <- aStart dest
                srcStart <- aStart src
                return $ durationAdd (durationDiff destStart srcStart) (durationTimes beatOff (beatLen bpm))

-- | The bootleg itself.
exampleBootleg :: Prog Audio
exampleBootleg = do
    acap <- audioTrack katy
    instr <- audioTrack walkOnBy
    warpedAcap <- warpTo instr acap
    shiftedAcap <- alignTo instr warpedAcap 16
    gainAcap <- gainAudio (Gain (-3)) shiftedAcap
    mergeAudio instr gainAcap
