-- | An optimizer for 'Prog' values

module Music.TMC.Optimize
    ( optimize
    ) where

import Music.TMC.Prog

-- | Optimize programs so that they can be compiled in less steps.
-- (or at worst, do nothing)
optimize :: Prog a -> Prog a
optimize = id
