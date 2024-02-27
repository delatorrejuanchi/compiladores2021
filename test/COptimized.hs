-- | C compilation test suite

module COptimized where

import           Spec

main :: IO ()
main =
    putStrLn "C Optimized Backend Test Suite" >>
    runTestWith "test/compile_and_run_c_optimized.sh "
