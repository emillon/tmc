-- | An optimizer for 'Prog' values

module Music.TMC.Optimize
    ( optimize
    ) where

import Control.Monad.Free
import Data.Maybe
import Music.TMC.Internals
import Music.TMC.Prog
import Music.TMC.Types

-- | This value can be passed to continuations, but should not be actually
-- evaluated.
optVal :: String -> Audio
optVal key =
    setBPM (error msgBPM) noAudio
        where
            msgBPM = "optVal (in " ++ key ++ ") : BPM evaluated"

-- | Optimize programs so that they can be compiled in less steps.
-- (or at worst, do nothing)
optimize :: Prog a -> Prog a
optimize (Prog p) = Prog $ optimizeF p

optimizeF :: Free ProgF a -> Free ProgF a
optimizeF = applyOpt optimizePads . applyOpt optimizeSeqs

type OptStep a = Free ProgF a -> Maybe (Free ProgF a)

applyOpt :: OptStep a -> Free ProgF a -> Free ProgF a
applyOpt step p =
    fromMaybe (optRest p) (step p)

-- | pad d1 (pad d2) A = pad (d1 + d2) A
optimizePads :: OptStep a
optimizePads p = do
    (d1, a, k0) <- extractPad p
    (d2, _, k) <- extractPad $ k0 $ optVal "optimizePads"
    return $ Free (Bind (OpSoxFX (SoxPad (durationAdd d1 d2)) a) k)

optRest :: Free ProgF a -> Free ProgF a
optRest p@(Pure _) = p
optRest (Free x) = Free $ fmap optimizeF x

extractPad :: Free ProgF a -> Maybe (Duration, Audio, Audio -> Free ProgF a)
extractPad (Free (Bind (OpSoxFX (SoxPad d) a) k)) = Just (d, a, k)
extractPad _ = Nothing

optimizeSeqs :: OptStep a
optimizeSeqs p = do
    (l1, k0) <- extractSeq p
    (l2, k) <- extractSeq $ k0 $ optVal "optimizeSeqs"
    let l = l1 ++ l2
    return $ Free (Bind (Sequence l) k)

extractSeq :: Free ProgF a -> Maybe ([Audio], Audio -> Free ProgF a)
extractSeq (Free (Bind (Sequence l) k)) = Just (l, k)
extractSeq _ = Nothing
