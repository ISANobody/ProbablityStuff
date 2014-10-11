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

  -- Binomial coefficient
  binom :: (Num a) => Int -> Int -> a
  binom n k = fromIntegral $ (fac n) `div` (fac k * (fac (n - k)))
  
  -- Do Map.adjust for an associative list
  assAdjust :: (Eq a) => (b -> b) -> a -> [(a,b)] -> [(a,b)]
  assAdjust f k l = map (\(k',x) -> if k == k' then (k',f x) else (k',x)) l

  -- Adjust a list at a given index
  -- Out of bounds indices do nothing (maybe this should error?)
  -- Would this be better as a fold?
  indAdjust :: Int -> (a -> a) -> [a] -> [a]
  indAdjust _ _ []     = []
  indAdjust 0 f (x:xs) = (f x):xs
  indAdjust n f (x:xs) = x:(indAdjust (n-1) f xs)
