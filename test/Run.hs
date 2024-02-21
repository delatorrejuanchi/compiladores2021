-- | Run test suite

module Run where

import           Spec

main :: IO ()
main =
    putStrLn "Run Test Suite" >>
    runTestWith "test/run.sh "

