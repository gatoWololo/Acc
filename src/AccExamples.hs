{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module AccExamples where

import Data.Array.Accelerate
import qualified Prelude as P
import Prelude((*), (+))

ind1 :: Int -> (Z :. Int)
ind1 n = Z :. n

array1 :: Vector Int
array1 = fromList (ind1 100000) [1, 2..]

dotProductSelf :: Acc (Vector Int) -> Acc (Scalar Int)
dotProductSelf xs = fold (+) 0 (zipWith (*) xs xs)

addTwenty :: Acc (Vector Int) -> Acc (Vector Int)
addTwenty xs = map (+ 20) xs

-- z = a * x + y
saxpy :: Exp Int -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int)
saxpy a x y =
  let ax = map (* a) x in
    zipWith (+) ax y
