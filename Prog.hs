module Prog ( Prog
            , ProgF(..)
            , Audio(..)
            , AudioType(..)
            , Op(..)
            , soxFX
            , warpAudio
            , shiftAudio
            , gainAudio
            , mergeAudio
            , Track(..)
            , audioTrack
            , opBPM
            ) where

import Control.Monad.Free

data AudioType = Mp3
               | Flac
    deriving (Show)

-- | Some Audio tracks have BPM, others do not.
data Audio = Audio { aPath :: FilePath
                   , aBPM :: Maybe Double
                   }
    deriving (Show)

data ProgF a = File Track (Audio -> a)
             | Bind Op (Audio -> a)

data Op = SoxFX String Audio
        | Merge Audio Audio

instance Functor ProgF where
    fmap f (File track k) = File track (f . k)
    fmap f (Bind op k) = Bind op (f . k)

type Prog a = Free ProgF a

soxFX :: String -> Audio -> Prog Audio
soxFX fx a = liftF $ Bind (SoxFX fx a) id

warpAudio :: Double -> Audio -> Prog Audio
warpAudio ratio = soxFX $ "tempo " ++ show ratio

shiftAudio :: Double -> Audio -> Prog Audio
shiftAudio amount = soxFX $ "pad " ++ show amount

gainAudio :: Double -> Audio -> Prog Audio
gainAudio amount = soxFX $ "gain " ++ show amount

mergeAudio :: Audio -> Audio -> Prog Audio
mergeAudio a b = liftF $ Bind (Merge a b) id

data Track = Track { trackFormat :: AudioType
                   , trackPath :: FilePath
                   , trackBPM :: Double
                   }

audioTrack :: Track -> Prog Audio
audioTrack t = liftF $ File t id

opBPM :: Op -> Maybe Double
opBPM (SoxFX sfx a) = aBPM a -- TODO check for different kinds of sfx
opBPM (Merge a b) = aBPM a -- we assume that we're mixing similar tracks
