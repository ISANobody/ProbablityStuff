{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Warmachine where
  import Numeric.Probability.Distribution hiding (map,filter)

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

  -- Takes def diff, arm diff, resource limit
  -- If this is too slow investigate dynamic programming
  maxExpected :: (Fractional p, Ord p) => Int -> Int -> Int -> p
  maxExpected d a 0 = expected $ do h <- attack d
                                    return $ if h
                                             then myExpected $ damage a
                                             else 0
  maxExpected d a n = maximum [undefined -- Buy Attack
                              ,undefined -- Boost Attack
                              ,undefined -- Boost Dmg only]
