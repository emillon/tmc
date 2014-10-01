
import Example
import Prog

main :: IO ()
main = do
    mapM_ putStrLn $ steps exampleBootleg
    f <- run exampleBootleg
    print f
