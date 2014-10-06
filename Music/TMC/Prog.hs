-- | The DSL itself.

module Music.TMC.Prog
    ( -- * Audio Tracks
      Audio
    , aPath
    , aBPM
    , aStart
      -- * The DSL
      -- ** Type
    , Prog
      -- ** Sources
    , Track(..)
    , AudioType(..)
    , audioTrack
    , synth
    , silence
      -- ** Operations
    , warpAudio
    , shiftAudio
    , gainAudio
    , mergeAudio
    , sequenceAudio
    , seqList
    , cutAudio
    ) where

import Control.Monad.Free

import Music.TMC.Internals
import Music.TMC.Types

soxFX :: SoxFX -> Audio -> Prog Audio
soxFX fx a = Prog $ liftF $ Bind (OpSoxFX fx a) id

-- | Change the speed of a track without altering its pitch
warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ SoxTempo ratio

-- | Pad with silence before the start.
shiftAudio :: Duration -> Audio -> Prog Audio
shiftAudio amount = soxFX $ SoxPad amount

-- | Adjust the gain (in DBs).
gainAudio :: Gain -> Audio -> Prog Audio
gainAudio amount = soxFX $ SoxGain amount

-- | Mix tracks together.
mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = Prog $ liftF $ Bind (Merge a b) id

-- | Read a 'Track'.
audioTrack :: Track -> Prog Audio
audioTrack t = Prog $ liftF $ (Bind $ File t) id

-- | Generate a sine wave.
synth :: Frequency -> Duration -> Prog Audio
synth freq dur = Prog $ liftF $ (Bind $ Synth freq dur) id

-- | Generate silence.
silence :: Duration -> Prog Audio
silence dur = Prog $ liftF $ (Bind$ Silence dur) id

-- | Join tracks (play one after another).
sequenceAudio :: Audio -> Audio -> Prog Audio
sequenceAudio a b = Prog $ liftF $ Bind (Sequence a b) id

-- | An extension of 'sequenceAudio' to lists.
seqList :: [Audio] -> Prog Audio
seqList [] = error "seqList"
seqList [a] = return a
seqList (a:as) = do
    r <- seqList as
    sequenceAudio a r

-- | Take only part of a track.
cutAudio :: Duration -- ^ Start
         -> Duration -- ^ End
         -> Audio -> Prog Audio
cutAudio start end = soxFX $ SoxTrim start end
