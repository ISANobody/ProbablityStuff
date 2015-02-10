{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Risk where
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Control.Monad
  import Control.Arrow
  import MathHammer
  
  d6 :: (Fractional prob, Num n, Enum n) => T prob n
  d6 = uniform [1..10]

  bunker :: (Fractional prob, Ord n, Num n, Enum n) => Int -> T prob (MultiSet n)
  bunker n = do rs <- independent n d6
                let (r,rs') = MS.deleteFindMax rs
                return (MS.insert (r+1) rs')

  eval :: MultiSet Int -> MultiSet Int -> (Int,Int)
  eval a d | MS.null a || MS.null d = (0,0)
  eval a d = let (i1,i2) = eval a' d' in if ra > rd then (i1,i2+1) else (i1+1,i2)
    where (ra,a') = MS.deleteFindMax a
          (rd,d') = MS.deleteFindMax d

  expected2 :: (Fractional prob, Integral n) => T prob (n,n) -> (prob,prob)
  expected2 d = (expected . fmap fromIntegral . fmap fst) *** (expected . fmap fromIntegral . fmap snd) $ (d,d)

  (???) :: (Num prob) => (a -> Bool) -> T prob (a, a) -> (prob, prob)
  (???) q d = ((q??fmap fst d),(q??fmap snd d))
