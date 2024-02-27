-- | RunCEK Optimized test suite

module RunCEKOptimized where

import           Spec

main :: IO ()
main =
    putStrLn "RunCEK Optimized Test Suite" >>
    runTestWith "test/run_cek_optimized.sh "
