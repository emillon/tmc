import Control.Monad
import System.Exit
import Test.HUnit

import Music.TMC.Optimize
import Music.TMC.Prog
import Music.TMC.Run
import Music.TMC.Types

main :: IO ()
main = do
    c <- runTestTT tests
    unless (isSuccess c) $ exitFailure

isSuccess :: Counts -> Bool
isSuccess c =
    errors c == 0 && failures c == 0

tests :: Test
tests = TestList ["Optimizer" ~: optimizeTests]

optimizeTests :: Test
optimizeTests = TestList $ concatMap makeOptTC tcs
    where
        makeOptTC (prog, snorm, sopt) =
            [ "Unoptimized" ~: snorm ~=? length (steps prog)
            , "Optimized" ~: sopt ~=? length (steps (optimize prog))
            ]
        da = Duration 1
        db = Duration 2
        tcs = [ (tc1, 2, 1)
              , (tc2, 3, 2)
              , (tc3, 4, 4)
              ]
        tc1 = do
            x <- shiftAudio da noAudio
            shiftAudio db x
        tc2 = do
            x <- gainAudio (Gain (-3)) noAudio
            y <- shiftAudio da x
            shiftAudio db y
        tc3 = replicateAudio 16 noAudio
