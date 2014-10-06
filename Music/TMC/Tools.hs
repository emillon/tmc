-- | Higher-level tools defined on programs.

module Music.TMC.Tools
    ( play
    , warpTo
    , alignTo
    ) where

import Control.Monad
import Data.Maybe
import System.Process

import Music.TMC.Prog
import Music.TMC.Types

-- | Change the tempo of a track (using 'warpAudio') so that it has the same
-- speed as a destination track.
warpTo :: Audio -- ^ Destination
       -> Audio -- ^ Source
       -> Prog Audio
warpTo dest src =
    warpAudio ratio src
        where
            ratio = fromMaybe (error "warpTo: no BPM on track") $ do
                destBPM <- aBPM dest
                origBPM <- aBPM src
                return $ bpmRatio destBPM origBPM

-- | Add padding at the beginning of a track so that the first beat coincides
-- with that of another track.
alignTo :: Audio -- ^ Destination
        -> Audio -- ^ Source
        -> Int -- ^ Beat offset
        -> Prog Audio
alignTo dest src beatOff =
    shiftAudio shiftAmount src
        where
            shiftAmount = fromMaybe (error "alignTo: missing a start time or BPM") $ do
                bpm <- aBPM dest
                destStart <- aStart dest
                srcStart <- aStart src
                return $ durationAdd (durationDiff destStart srcStart) (durationTimes beatOff (beatLen bpm))

-- | Play audio.
play :: Audio -> IO ()
play a =
    void $ rawSystem "mpv" [aPath a]
