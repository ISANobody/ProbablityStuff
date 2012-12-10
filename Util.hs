{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where
  power :: (Num a) => a -> Int -> a
  power _ 0 = 1
  power n 1 = n
  power n 2 = n * n
  power n k = (power n r) * (power (power n d) 2)
        where (d,r) = divMod k 2

  fac :: Int -> Integer
  fac i = product [1..fromIntegral i]

  multinom :: [Int] -> Integer
  multinom ns = (fac . sum $ ns) `div` (product . map fac $ ns)
