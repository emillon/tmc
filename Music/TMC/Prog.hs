-- | The DSL itself.

module Music.TMC.Prog
    ( -- * Audio Tracks
      Audio
    , aPath
    , aBPM
    , aStart
    , noAudio
    , setBPM
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
    , replicateAudio
    , cutAudio
    ) where

import Control.Monad.Free

import Music.TMC.Cache
import Music.TMC.Internals
import Music.TMC.Types

-- | A dummy audio track.
noAudio :: Audio
noAudio =
    Audio
        { aCache = error "noAudio cache"
        , aBPM = error "noAudio BPM"
        , aStart = error "noAudio start"
        }

-- | Where this track is stored.
aPath :: Audio -> FilePath
aPath = coFile . aCache

-- | Override BPM info for an 'Audio' object.
setBPM :: Maybe BPM -> Audio -> Audio
setBPM bpm a = a { aBPM = bpm }

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
sequenceAudio a b = seqList [a, b]

-- | An extension of 'sequenceAudio' to lists.
seqList :: [Audio] -> Prog Audio
seqList l = Prog $ liftF $ Bind (Sequence l) id

-- | N times the same sound.
replicateAudio :: Int -> Audio -> Prog Audio
replicateAudio n a = seqList $ replicate n a

-- | Take only part of a track.
cutAudio :: Duration -- ^ Start
         -> Duration -- ^ End
         -> Audio -> Prog Audio
cutAudio start end = soxFX $ SoxTrim start end
