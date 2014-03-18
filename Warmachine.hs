{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Warmachine where
  import Numeric.Probability.Distribution hiding (map,filter)
  import MathHammer hiding (roll,_3d6)
  import Data.Function.Memoize
  import Data.List
  import Debug.Trace
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)

  data Weapon = Weapon { pow :: Int }
  deriveMemoizable ''Weapon

  data Warjack = Warjack { str :: Int
                         , mat :: Int
                         , def :: Int
                         , arm :: Int
                         , boxes :: Int
                         , weapon :: Weapon }
  deriveMemoizable ''Warjack

  leviathan :: Warjack
  leviathan = Warjack { str = 12
                      , mat = 6
                      , def = 12
                      , arm = 18
                      , boxes = 30
                      , weapon = Weapon 5 }

  -- Fixes some type class generality
  myExpected :: (Fractional p, Integral n) => T p n -> p
  myExpected = expected . (fmap fromIntegral)


  -- Underscores because numbers can't start identifiers
  _1d6,_2d6,_3d6 :: (Fractional prob, Ord prob) => T prob Int
  _1d6 = norm $ uniform [1..6]
  _2d6 = norm $ _1d6 + _1d6
  _3d6 = norm $ _1d6 + _1d6 + _1d6

  -- Takes in the difference
  attack :: (Fractional prob, Ord prob) => Int -> T prob Bool
  attack n = norm $ do r <- _2d6
                       return $ (r /= 2) && (r+n > 0 || r == 12)

  boostedAtk :: (Fractional prob, Ord prob) => Int -> T prob Bool
  boostedAtk n = norm $ do r <- _3d6
                           return $ (r /= 3) && (r+n > 0 || r == 18)

  damage :: (Fractional prob, Ord prob) => Int -> T prob Int
  damage n = norm $ do r <- _2d6
                       return $ max (r+n) 0

  boostedDmg :: (Fractional prob, Ord prob) => Int -> T prob Int
  boostedDmg n = norm $ do r <- _3d6
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

  -- wider memo tracing function
  traceMemoize2 :: (Show a, Show b, Memoizable a, Memoizable b) => (a -> b -> c) -> (a -> b -> c)
  traceMemoize2 f = memoize2 (\a b -> trace (show a ++ " " ++ show b) (f a b))
  traceMemoize3 :: (Show a, Show b, Show c, Memoizable a, Memoizable b, Memoizable c) 
     => (a -> b -> c -> d) -> (a -> b -> c -> d)
  traceMemoize3 f = memoize3 (\a b c -> trace (show a ++ " " ++ show b ++ " " ++ show c) (f a b c))
  traceMemoize4 :: (Show a, Show b, Show c, Show d,
                    Memoizable a, Memoizable b, Memoizable c, Memoizable d) 
     => (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> e)
  traceMemoize4 f = memoize4 (\a b c d -> trace (show a ++ " " ++ show b
                                                ++ " " ++ show c ++ " " ++ show d) (f a b c d))
  traceMemoize5 :: (Show a, Show b, Show c, Show d, Show e,
                    Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e) 
     => (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> f)
  traceMemoize5 m = memoize5 (\a b c d  e -> 
                    trace (show a ++ " " ++ 
                           show b ++ " " ++ 
                           show c ++ " " ++ 
                           show d ++ " " ++
                           show e) 
                          (m a b c d e))

  --  A wound transfer strategy for dealing with ecaine takes 
  -- boxes left, dmg from the current hit, shots remaining, num of transfers
  -- and function for the probability of success, returns whether to transfer
  -- I wonder if we should allow for randomized strategy
  type TransferStrat p = Int -> Int -> Int -> Int -> 
       (Int -> Int -> Int -> T p Bool)
       -> Bool

  alwaysTransfer,neverTransfer,avoidDeath :: TransferStrat p
  neverTransfer _ _ _ _ _ = False
  alwaysTransfer _ _ _ 0 _ = False
  alwaysTransfer _ _ _ _ _ = True
  avoidDeath b dmg _ _ _ = dmg >= b

  minExpectedTransfer :: (Fractional p, Ord p) => TransferStrat p
  minExpectedTransfer b dmg s t f = (id ?? f t s (max 0 (b-dmg))) > (id ?? f (t-1) s b)


  -- takes a transfer strategy, relative defense, relative armor, number of transfers, number
  -- of shots remaining, boxes left, and returns a distribution of the number of boxes left
  ecaine :: (Fractional p, Ord p) => TransferStrat p -> Int -> Int -> Int -> Int -> Int -> T p Bool
  ecaine w = 
   let f = memoize5 $ \d a t s b -> 
         case s of
         0 -> certainly (b<=0)
         _ -> do h <- attack d
                 if h
                 then do dmg <- damage a
                         if t > 0 && w b dmg (s-1) t (f d (a+1))
                         then norm $ f d (a+1) (t-1) (s-1) b
                         else if dmg >= b
                              then return True
                              else norm $ f d (a+1) t (s-1) (b-dmg)
                 else norm $ f d a t (s-1) b
   in f

  -- Target Sum, Largest summand
  intPartition :: Int -> [[Int]]
  intPartition i = go i i
    where go :: Int -> Int -> [[Int]]
          go 0 _ = [[]]
          go 1 _ = [[1]]
          go n m = do x <- [1..(min m n)]
                      xs <- go (n-x) x
                      return $ x:xs

  intPartitionMS :: Int -> [MultiSet Int]
  intPartitionMS i = go i i
    where go :: Int -> Int -> [MultiSet Int]
          go 0 _ = [MS.empty]
          go 1 _ = [MS.singleton 1]
          go n m = do x <- [1..(min m n)]
                      xs <- go (n-x) x
                      return $ MS.insert x xs

  -- Avoid recomputation of the measure
  -- Fails on empty lists
  maxWRTMemo :: forall a b. Ord b => (a -> b) -> [a] -> a
  maxWRTMemo _ [] = undefined
  maxWRTMemo _ [x] = x
  maxWRTMemo f (x:xs) = go (f x) x xs
    where go :: b -> a -> [a] -> a
          go _ e [] = e
          go m e (y:ys) = let m' = f y
                          in if m' > m
                             then go m' y ys
                             else go m e ys

  craPosition :: (Fractional p, Ord p) => Int -> Int -> [Int] -> T p Int
  craPosition _ _ [] = certainly 0
  craPosition d a (1:ns) = do h <- attack d
                              if h
                              then damage a + craPosition d a ns
                              else craPosition d a ns
  craPosition d a (n:ns) = do h <- attack (d+n)
                              if h
                              then damage (a+n) + craPosition d a ns
                              else craPosition d a ns

  craPositionEx :: Int -> Int -> [Int] -> Double
  craPositionEx _ _ [] = 0
  craPositionEx d a (1:ns) = expected $ do h <- attack d
                                           if h
                                           then return $ (myExpected $ damage a) + craPositionEx d a ns
                                           else return $ craPositionEx d a ns
  craPositionEx d a (n:ns) = expected $ do h <- attack (d+n)
                                           if h
                                           then return $ (myExpected $ damage (a+n)) + craPositionEx d a ns
                                           else return $ craPositionEx d a ns

  -- Use linearity of expectation
  craPosMSEx :: Int -> Int -> MultiSet Int -> Double
  craPosMSEx d a ms = MS.foldOccur go 0 ms
    where go :: Int -> Int -> Double -> Double
          go 1 o acc = acc + (fromIntegral o)  
                             * (expected $ do h <- attack d
                                              if h
                                              then return . myExpected . damage $ a
                                              else return 0)
          go n o acc = acc + (fromIntegral o)  
                             * (expected $ do h <- attack $ d+n
                                              if h
                                              then return . myExpected . damage $ a+n
                                              else return 0)

  -- CRA takes def diff, armor diff, and number of models, returns damage distribution
  cra :: Int -> Int -> Int -> MultiSet Int
  cra d a s = maxWRTMemo (craPosMSEx d a) (intPartitionMS s)

  expectedDMGMS :: (Fractional p, Ord p) => T p (MultiSet Int) -> p
  expectedDMGMS = expected . fmap (MS.foldOccur (\i o a -> fromIntegral (i*o) + a) 0)

  expectedOneBoxed :: (Fractional p, Ord p) => T p (MultiSet Int) -> p
  expectedOneBoxed = myExpected . fmap (MS.size . MS.filter (>0))

  -- def, arm, returns distribution over damage assuming no aim
  fullCharger :: (Fractional p, Ord p) => Int -> Int -> T p (MultiSet Int)
  fullCharger d a = do h1 <- boostedAtk (6-d)
                       h2 <- boostedAtk (6-d)
                       d1 <- boostedDmg (12-a)
                       d2 <- boostedDmg (12-a)
                       return $ MS.insert (if h1 then d1 else 0) (MS.singleton $ if h2 then d2 else 0)

  fullSent :: forall p. (Fractional p, Ord p) => Int -> Int -> T p (MultiSet Int)
  fullSent d a = do s <- uniform [1..6]
                    go s 3
     where go :: Int -> Int -> T p (MultiSet Int)
           go = let rec = memoize2 $ \s f -> case (s,f) of
                          (_,0) -> norm $ independentRepFold MS.union (\x n -> MS.insertMany x n MS.empty) s 
                                                             (damage (10-a))
                          (1,1) -> do h <- attack (6-d)
                                      p <- boostedDmg (10-a)
                                      return . MS.singleton $ if h then p else 0
                          (1,_) -> do h <- boostedAtk (6-d)
                                      p <- boostedDmg (10-a)
                                      return . MS.singleton $ if h then p else 0
                          (_,_) -> do h <- attack (6-d)
                                      if h
                                      then do pr <- rec (s-1) (f-1)
                                              p  <- boostedDmg (10-a)
                                              return $ MS.insert p pr
                                      else rec (s-1) (f-1)
                in rec
