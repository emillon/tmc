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
optimize = optimizePads

-- | pad d1 (pad d2) A = pad (d1 + d2) A
optimizePads :: Prog a -> Prog a
optimizePads noopt@(Prog p) =
    fromMaybe noopt $ do
        (d1, a, k0) <- extractPad p
        (d2, _, k) <- extractPad (k0 noAudio)
        return $ Prog (Free (Bind (OpSoxFX (SoxPad (durationAdd d1 d2)) a) k))

extractPad :: Free ProgF a -> Maybe (Duration, Audio, Audio -> Free ProgF a)
extractPad (Free (Bind (OpSoxFX (SoxPad d) a) k)) = Just (d, a, k)
extractPad _ = Nothing
