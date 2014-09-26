module Prog ( Prog
            , ProgF(..)
            , Audio(..)
            , AudioType(..)
            , Op(..)
            , sourceFile
            , soxFX
            , warpAudio
            , shiftAudio
            , gainAudio
            , mergeAudio
            ) where

import Control.Monad.Free

data AudioType = Mp3
               | Flac
    deriving (Show)

data Audio = Audio FilePath
    deriving (Show)

data ProgF a = File AudioType FilePath (Audio -> a)
             | Bind Op (Audio -> a)

data Op = SoxFX String Audio
        | Merge Audio Audio

instance Functor ProgF where
    fmap f (File typ path k) = File typ path (f . k)
    fmap f (Bind op k) = Bind op (f . k)

type Prog a = Free ProgF a

sourceFile :: AudioType -> FilePath -> Prog Audio
sourceFile typ fp = liftF $ File typ fp id

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
