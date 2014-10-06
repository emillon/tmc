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
optimizeTests = TestList $ concatMap makeOptTC [(tc1, 2, 1)]
    where
        makeOptTC (prog, snorm, sopt) =
            [ snorm ~=? length (steps prog)
            , sopt ~=? length (steps (optimize prog))
            ]
        da = Duration 1
        db = Duration 2
        tc1 = do
            x <- shiftAudio da noAudio
            shiftAudio db x
