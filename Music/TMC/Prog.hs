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
      -- ** Tools on operations
    , opBPM
    , opStart
    ) where

import Control.Applicative
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

-- | Compute the BPM associated to the output of an operation.
opBPM :: Op -> Maybe BPM
opBPM (File track) = Just $ trackBPM track
opBPM (Synth _ _) = Nothing
opBPM (Silence _) = Nothing
opBPM (OpSoxFX sfx a) = soxBPM sfx <$> aBPM a
opBPM (Merge a _b) = aBPM a -- we assume that we're mixing similar tracks
opBPM (Sequence _) = Nothing -- could optimize when all in the list are close

soxBPM :: SoxFX -> BPM -> BPM
soxBPM (SoxTempo ratio) (BPM x) = BPM $ ratio * x
soxBPM (SoxPad _) x = x
soxBPM (SoxGain _) x = x
soxBPM (SoxTrim _ _) x = x

-- | Compute the Start time associated to the output of an operation.
opStart :: Op -> Maybe Duration
opStart (File track) = Just $ trackStart track
opStart (Synth _ _) = Just $ Duration 0
opStart (Silence _) = Nothing
opStart (OpSoxFX sfx a) = do
    sa <- aStart a
    soxStart sfx sa
opStart (Merge a _b) = aStart a -- we assume that we're mixing aligned tracks
opStart (Sequence _) = Nothing -- could optimize when all in the list are close

soxStart :: SoxFX -> Duration -> Maybe Duration
soxStart (SoxTempo ratio) (Duration x) = Just $ Duration $ ratio * x
soxStart (SoxPad shift) x = Just $ durationAdd shift x
soxStart (SoxGain _) x = Just x
soxStart (SoxTrim _ _) _ = Nothing -- maybe it's possible to compute it
