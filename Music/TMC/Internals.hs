{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | How TMC works under the hood.
-- Most of the types defined here are re-exported through 'Music.TMC.Prog', but
-- in order to keep their constructors hidden, they have to be defined here.

module Music.TMC.Internals
    ( Audio(..)
    , AudioType(..)
    , Op(..)
    , Prog(..)
    , ProgF(..)
    , SoxFX(..)
    , Track(..)
    ) where

import Control.Monad.Free

import Music.TMC.Cache
import Music.TMC.Types

-- | The free functor that enables us to create 'Prog'.
data ProgF a = Bind Op (Audio -> a)

instance Functor ProgF where
    fmap f (Bind op k) = Bind op (f . k)

-- | Our main Monad.
newtype Prog a = Prog (Free ProgF a)
    deriving (Monad)

-- | An operation on audio files.
-- Basically it's the non-monadic interface behind 'Prog'.
data Op = File Track
        | Synth Frequency Duration
        | Silence Duration
        | OpSoxFX SoxFX Audio
        | Merge Audio Audio
        | Sequence [Audio]

-- | A SoX effect.
data SoxFX = SoxTempo Double
           | SoxPad Duration
           | SoxGain Gain
           | SoxTrim Duration Duration

-- | A bounced audio track.
-- This corresponds to an intermediate WAV file with
-- metadata such as BPM or start time.
data Audio = Audio { aCache :: CObject
                   , aBPM :: Maybe BPM -- ^ Retrieve the BPM of an audio track
                   , aStart :: Maybe Duration -- ^ Retrieve the start time of an audio track
                   }
    deriving (Show)

-- | An audio file on disk. 'trackStart' is the position of beat zero.
data Track = Track { trackFormat :: AudioType
                   , trackPath :: FilePath
                   , trackBPM :: BPM
                   , trackStart :: Duration
                   }
    deriving (Show)

-- | Type of an audio source
data AudioType = Mp3
               | Flac
    deriving (Show)
