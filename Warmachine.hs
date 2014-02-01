{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Warmachine where
  import Numeric.Probability.Distribution hiding (map,filter)
  import Data.Function.Memoize

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

  basicAtkBA :: (Fractional prob, Ord prob) => Int -> Int -> Int -> T prob prob
  basicAtkBA _ _ 0 = error "Can't boost attack with no resource"
  basicAtkBA d a 1 = do h <- boostedAtk d
                        return $ if h then (myExpected $ damage a) else 0
  basicAtkBA d a r = do h <- boostedAtk d
                        return $ (memoed d a (r-2)) + if h 
                                                           then (myExpected $ damage a) 
                                                           else 0

  basicAtkBD :: (Fractional prob, Ord prob) => Int -> Int -> Int -> T prob prob
  basicAtkBD _ _ 0 = error "Can't boost damage with no resource"
  basicAtkBD d a 1 = do h <- attack d
                        return $ if h then (myExpected $ boostedDmg a) 
                                      else memoed d a 0
  basicAtkBD d a r = do h <- attack d
                        return $ if h 
                                 then (myExpected $ boostedDmg a) + (memoed d a (r-2))
                                 else memoed d a (r-1)

  basicAtkBABD :: (Fractional prob, Ord prob) => Int -> Int -> Int -> T prob prob
  basicAtkBABD _ _ 0 = error "Can't boost atk+dmg with no resource"
  basicAtkBABD _ _ 1 = error "Can't boost atk+dmg with one resource"
  basicAtkBABD d a 2 = do h <- boostedAtk d
                          return $ if h 
                                   then (myExpected $ boostedDmg a) 
                                   else memoed d a 0
  basicAtkBABD d a r = do h <- boostedAtk d
                          return $ if h 
                                   then (myExpected $ boostedDmg a) + (memoed d a (r-3))
                                   else memoed d a (r-2)

  -- Takes def diff, arm diff, resource limit
  -- If this is too slow investigate dynamic programming
  -- We assume that we currently have one attack available and are trying
  -- to optimize the use of our resource
  maxExpected :: (Fractional p, Ord p) => Int -> Int -> Int -> p
  maxExpected d a 0 = myExpected $ basicAtk d a
  maxExpected d a 1 = maximum [(myExpected $ basicAtk d a) + memoed d a 0
                              ,expected $ basicAtkBA d a 1
                              ,expected $ basicAtkBD d a 1]
  maxExpected d a r = maximum [(myExpected $ basicAtk d a) + memoed d a (r-1)
                              ,expected $ basicAtkBA d a r
                              ,expected $ basicAtkBD d a r
                              ,expected $ basicAtkBABD d a r]

  memoed :: (Fractional p, Ord p) => Int -> Int -> Int -> p
  memoed d a r = memoize (maxExpected d a) r
