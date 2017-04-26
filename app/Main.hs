module Main where

import AccExamples
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate

main :: IO ()
main = do
  let myArray = run (dotProductSelf (use array1))
    in putStrLn (show myArray)
