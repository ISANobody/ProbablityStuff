{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Warmachine where
  import Numeric.Probability.Distribution hiding (map,filter)
  import Data.Function.Memoize
  import Data.List

  -- Fixes some type class generality
  myExpected :: (Fractional p, Integral n) => T p n -> p
  myExpected = expected . (fmap fromIntegral)

  -- Underscores because numbers can't start identifiers
  _1d6,_2d6,_3d6 :: (Fractional prob, Ord prob) => T prob Int
  _1d6 = uniform [1..6]
  _2d6 = _1d6 + _1d6
  _3d6 = _1d6 + _1d6 + _1d6

  -- Takes in the difference
  attack :: (Fractional prob, Ord prob) => Int -> T prob Bool
  attack n = do r <- _2d6
                return $ r+n > 0

  boostedAtk :: (Fractional prob, Ord prob) => Int -> T prob Bool
  boostedAtk n = do r <- _3d6
                    return $ r+n > 0

  damage :: (Fractional prob, Ord prob) => Int -> T prob Int
  damage n = do r <- _2d6
                return $ max (r+n) 0

  boostedDmg :: (Fractional prob, Ord prob) => Int -> T prob Int
  boostedDmg n = do r <- _3d6
                    return $ max (r+n) 0

  basicAtk :: (Fractional prob, Ord prob) => Int -> Int -> T prob Int
  basicAtk d a = do h <- attack d
                    if h then damage a else 0

  data Action = None | BoostAtk | BoostDmg | BoostBoth deriving (Ord, Eq, Show, Enum)

  -- Should be in Prelude :(
  maximumWRT :: (Ord b) => (a -> b) -> [a] -> a
  maximumWRT f xs = maximumBy (\x y -> compare (f x) (f y)) xs

  -- This is ugly. I don't know why memoization didn't work on the cleaner version
  memoed :: (Fractional p, Ord p) => Int -> Int -> Int -> (Action,p)
  memoed d a = 
    let f = traceMemoize $ \r -> case r of
                                 0 -> (None,myExpected $ basicAtk d a)
                                 1 -> maximumWRT snd [(None,(myExpected $ basicAtk d a) + (snd $ f 0))
                                                     ,(BoostAtk,expected (batk 1 f))
                                                     ,(BoostDmg,expected (bdmg 1 f))]
                                 _ -> maximumWRT snd [(None,(myExpected $ basicAtk d a) + (snd $ f (r-1)))
                                                     ,(BoostAtk,expected (batk r f))
                                                     ,(BoostDmg,expected (bdmg r f))
                                                     ,(BoostBoth,expected (both r f))]
    in f
   where batk 0 _ = error "Can't boost attack with no resource"
         batk 1 _ = do h <- boostedAtk d
                       return $ if h then (myExpected $ damage a) else 0
         batk r f = do h <- boostedAtk d
                       return $ (snd $ f (r-2)) + if h 
                                            then (myExpected $ damage a) 
                                            else 0
         bdmg 0 _ = error "Can't boost damage with no resource"
         bdmg 1 f = do h <- attack d
                       return $ if h then (myExpected $ boostedDmg a) 
                                     else snd $ f 0
         bdmg r f = do h <- attack d
                       return $ if h 
                                then (myExpected $ boostedDmg a) + (snd $ f (r-2))
                                else snd $ f (r-1)
         both 0 _ = error "Can't boost atk+dmg with no resource"
         both 1 _ = error "Can't boost atk+dmg with one resource"
         both 2 f = do h <- boostedAtk d
                       return $ if h 
                                then (myExpected $ boostedDmg a) 
                                else snd $ f 0
         both r f = do h <- boostedAtk d
                       return $ if h 
                                then (myExpected $ boostedDmg a) + (snd $ f (r-3))
                                else snd $ f (r-2)
