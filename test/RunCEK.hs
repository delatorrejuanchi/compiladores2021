-- | RunCEK test suite

module RunCEK where

import           Spec

main :: IO ()
main =
    putStrLn "RunCEK Test Suite" >>
    runTestWith "test/run_cek.sh "
