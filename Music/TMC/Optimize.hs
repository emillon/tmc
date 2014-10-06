-- | An optimizer for 'Prog' values

module Music.TMC.Optimize
    ( optimize
    ) where

import Control.Monad.Free
import Data.Maybe
import Music.TMC.Internals
import Music.TMC.Run (noAudio)
import Music.TMC.Types

-- | Optimize programs so that they can be compiled in less steps.
-- (or at worst, do nothing)
optimize :: Prog a -> Prog a
optimize (Prog p) = Prog $ optimizeF p

optimizeF :: Free ProgF a -> Free ProgF a
optimizeF = applyOpt optimizePads

type OptStep a = Free ProgF a -> Maybe (Free ProgF a)

applyOpt :: OptStep a -> Free ProgF a -> Free ProgF a
applyOpt step p =
    fromMaybe (optRest p) (step p)

-- | pad d1 (pad d2) A = pad (d1 + d2) A
optimizePads :: OptStep a
optimizePads p = do
    (d1, a, k0) <- extractPad p
    (d2, _, k) <- extractPad (k0 noAudio)
    return $ Free (Bind (OpSoxFX (SoxPad (durationAdd d1 d2)) a) k)

optRest :: Free ProgF a -> Free ProgF a
optRest p@(Pure _) = p
optRest (Free x) = Free $ fmap optimizeF x

extractPad :: Free ProgF a -> Maybe (Duration, Audio, Audio -> Free ProgF a)
extractPad (Free (Bind (OpSoxFX (SoxPad d) a) k)) = Just (d, a, k)
extractPad _ = Nothing
