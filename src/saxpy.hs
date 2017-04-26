{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Saxpy where

import Data.Array.Accelerate
import qualified Prelude as P

-- Entries in array:
entries :: Exp Int
entries = 10000


-- Kernel function to compure saxpy.
-- z = a * x + y
saxpy :: Exp Int -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int)
saxpy a x y = let ax = map (* a) x in
                zipWith (+) ax y

makeArr n = generate (index1 n) (\ ind -> unindex1 ind)

saxpyMain :: P.IO ()
saxpyMain = do
  let arr1 = makeArr entries
      arr2 = makeArr entries
      result = saxpy 123 arr1 arr2 in
    P.return ()
