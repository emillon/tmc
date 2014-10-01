-- | Main module.

module Main (main) where

import Example
import Prog

-- | Run the program!
main :: IO ()
main = do
    mapM_ putStrLn $ steps exampleBootleg
    f <- run exampleBootleg
    print f
