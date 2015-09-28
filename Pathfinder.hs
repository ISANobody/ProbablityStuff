{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pathfinder where
  import MathHammer
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Data.Ratio
  import Data.List
  import Data.Function.Memoize

  d6,d8 :: (Fractional prob) => T prob Integer
  d6 = uniform [1..6]
  d8 = uniform [1..8]

  -- This probably has a better name
  data SGTag a = Pos a | Neg a deriving (Show,Eq)
  type SGExp a = [SGTag a]

  sgmult :: (Fractional a) => SGExp a -> a
  sgmult = product . map go 
    where go (Pos x) = x
          go (Neg x) = 1/x

  sgadd :: (Num a) => SGExp a -> a
  sgadd = sum . map go
    where go (Pos x) = x
          go (Neg x) = negate x

  sgsolutions :: MultiSet Integer -> [Integer]
  sgsolutions = nub . map numerator . filter ((==1) . denominator) . sgsolutions'
             . MS.elems

  sgsolutions' :: [Integer] -> [Ratio Integer]
  sgsolutions' = 
    let f :: [Integer] -> [Ratio Integer]
        f = memoize $ \xs -> case xs of
             [x] -> [fromInteger x]
             _ -> do (lms,rms) <- (filter eitherempty . mssplits . MS.fromList . map fromInteger $ xs)
                     l <- nub $ f (MS.elems lms)
                     r <- nub $ f (MS.elems rms)
                     if r == 0
                     then [l*r,l+r,l-r]
                     else [l*r,l/r,l+r,l-r]
    in f
    where eitherempty :: (MultiSet a,MultiSet a) -> Bool
          eitherempty (l,r) = not (MS.null l) && not (MS.null r)

  sggoals :: (Eq a,Num a) => a -> [a]
  sggoals 1 = [3,5,7]
  sggoals 2 = [11,13,17]
  sggoals 3 = [19,23,29]
  sggoals 4 = [31,37,41]
  sggoals 5 = [43,47,53]
  sggoals 6 = [59,61,67]
  sggoals 7 = [71,73,79]
  sggoals 8 = [83,89,97]
  sggoals 9 = [101,103,107]
  sggoals _ = undefined

  overlap :: (Eq a) => [a] -> [a] -> Bool
  overlap xs ys = any (flip elem $ ys) xs

  -- Ranks in Engineering -> Goal lvl -> Result
  sgprob :: (Fractional prob) => Int -> Integer -> T prob Bool
  sgprob k l = do r <- norm $ independent k d6
                  return (overlap (sggoals l) (sgsolutions r))
