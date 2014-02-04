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

  data Action = None | BoostAtk | BoostDmg | BoostBoth
              | HeadButt | HeadBAtk | HeadBDmg | HeadBoth
               deriving (Ord, Eq, Show, Enum)

  -- Should be in Prelude :(
  maximumWRT :: (Ord b) => (a -> b) -> [a] -> a
  maximumWRT f xs = maximumBy (\x y -> compare (f x) (f y)) xs

  -- This is ugly. I don't know why memoization didn't work on the cleaner version
  normalAtk :: (Fractional p, Ord p) => Int -> Int -> Int -> Bool -> Int -> (Action,p)
  normalAtk s d a = 
    let f = memoize2 $ \ah r -> case r of
                                 0 -> (None,expected (natk ah 0 f))
                                 1 -> maximumWRT snd [(None,(expected (natk ah 1 f)))
                                                     ,(BoostAtk,expected (batk ah 1 f))
                                                     ,(BoostDmg,expected (bdmg ah 1 f))
                                                     ,(HeadButt,expected (hdbt ah 1 f))]
                                 2 -> maximumWRT snd [(None,(expected (natk ah r f)))
                                                     ,(BoostAtk,expected (batk ah r f))
                                                     ,(BoostDmg,expected (bdmg ah r f))
                                                     ,(BoostBoth,expected (both ah r f))
                                                     ,(HeadButt,expected (hdbt ah r f))
                                                     ,(HeadBAtk,expected (hdba ah r f))
                                                     ,(HeadBDmg,expected (hdbd ah r f))]
                                 _ -> maximumWRT snd [(None,(expected (natk ah r f)))
                                                     ,(BoostAtk,expected (batk ah r f))
                                                     ,(BoostDmg,expected (bdmg ah r f))
                                                     ,(BoostBoth,expected (both ah r f))
                                                     ,(HeadButt,expected (hdbt ah r f))
                                                     ,(HeadBAtk,expected (hdba ah r f))
                                                     ,(HeadBDmg,expected (hdbd ah r f))
                                                     ,(HeadBoth,expected (hdda ah r f))]
    in f
   where natk ah 0 _ = do h <- attack d
                          return $ if h || ah then (myExpected $ damage a) else 0
         natk ah r f = do h <- attack d
                          return $ if h || ah then (myExpected $ damage a) + (snd $ f ah (r-1)) 
                                              else (snd $ f ah (r-1))
         batk _ 0 _ = error "Can't boost attack with no resource"
         batk ah 1 _ = do h <- boostedAtk d
                          return $ if h || ah then (myExpected $ damage a) else 0
         batk ah r f = do h <- boostedAtk d
                          return $ (snd $ f ah (r-2)) + if h || ah
                                            then (myExpected $ damage a) 
                                            else 0
         bdmg _ 0 _ = error "Can't boost damage with no resource"
         bdmg ah 1 f = do h <- attack d
                          return $ if h || ah then (myExpected $ boostedDmg a) 
                                              else snd $ f ah 0
         bdmg ah r f = do h <- attack d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg a) + (snd $ f ah (r-2))
                                   else snd $ f ah (r-1)
         both _ 0 _ = error "Can't boost atk+dmg with no resource"
         both _ 1 _ = error "Can't boost atk+dmg with one resource"
         both ah 2 f = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg a) 
                                   else snd $ f ah 0
         both ah r f = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg a) + (snd $ f ah (r-3))
                                   else snd $ f ah (r-2)
         hdbt _ 0 _ = error "Can't power attack with no resource"
         hdbt ah 1 _ = do h <- attack d
                          return $ if h || ah
                                   then (myExpected $ damage s)
                                   else 0
         hdbt ah r f = do h <- attack d
                          return $ if h || ah
                                   then (myExpected $ damage s) + (snd $ f True (r-1))
                                   else snd $ f ah (r-1)
         hdba _ 0 _ = error "Can't power attack+boost attack with no resource"
         hdba _ 1 _ = error "Can't power attack+boost attack with one resource"
         hdba ah 2 _ = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ damage s)
                                   else 0
         hdba ah r f = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ damage s) + (snd $ f True (r-2))
                                   else snd $ f ah (r-2)
         hdbd _ 0 _ = error "Can't power attack+boost damage with no resource"
         hdbd _ 1 _ = error "Can't power attack+boost damage with one resource"
         hdbd ah 2 f = do h <- attack d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg s)
                                   else snd $ f ah 1
         hdbd ah r f = do h <- attack d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg s) + (snd $ f True (r-2))
                                   else snd $ f ah (r-1)
         hdda _ 0 _ = error "Can't power attack+boost both with no resource"
         hdda _ 1 _ = error "Can't power attack+boost both with one resource"
         hdda _ 2 _ = error "Can't power attack+boost both with two resource"
         hdda ah 3 f = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg s)
                                   else snd $ f ah 1
         hdda ah r f = do h <- boostedAtk d
                          return $ if h || ah
                                   then (myExpected $ boostedDmg s) + (snd $ f True (r-3))
                                   else snd $ f ah (r-2)
