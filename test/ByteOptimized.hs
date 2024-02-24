module ByteOptimized where

-- | Bytecode Test Suite
import           Spec

main :: IO ()
main =
  putStrLn "Byte Optimized Test Suite" >>
  runTestWith "test/byte_run_optimized.sh "
