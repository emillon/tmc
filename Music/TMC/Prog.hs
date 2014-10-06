-- | The DSL itself.

module Music.TMC.Prog
    ( -- * Audio Tracks
      Audio
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
      -- * Tools
    , metronome
    , checkBPM
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

-- | A track that only does beep beep.
-- There are actually two different beeps, a high one and a low one.
-- It does H L L L H L L L... (4/4)
metronome :: BPM
          -> Int -- ^ Number of bars
          -> Prog Audio
metronome bpm nbars = do
    hiBeep <- synth hiFreq beepLen
    sil <- silence silenceLen
    loBeep <- synth loFreq beepLen
    bar <- seqList [hiBeep, sil, loBeep, sil, loBeep, sil, loBeep, sil]
    seqList $ replicate nbars bar
        where
            beepLen = Duration 0.1
            hiFreq = Frequency $ 2 * baseFreq
            loFreq = Frequency baseFreq
            baseFreq = 440
            silenceLen = durationDiff (beatLen bpm) beepLen

-- | Play a metronome synchronized on top of a track.
-- It should be an easy way to know if the track's metadata is correct.
checkBPM :: Track -> Prog Audio
checkBPM track = do
    a <- audioTrack track
    m <- metronome (trackBPM track) 16
    s <- shiftAudio (trackStart track) m
    mergeAudio a s
