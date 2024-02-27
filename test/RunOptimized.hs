-- | RunOptimized test suite

module RunOptimized where

import           Spec

main :: IO ()
main =
    putStrLn "RunOptimized Test Suite" >>
    runTestWith "test/run_optimized.sh "

