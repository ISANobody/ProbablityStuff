{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeOfEmpire where
  import MathHammer hiding (roll)
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Data.Ratio
  import Data.List

  -- Fixes some type class generality
  myExpected :: (Fractional p, Integral n) => T p n -> p
  myExpected = expected . (fmap fromIntegral)

  -- Bad name (Success,Advantage)
  -- Treat Triumphs as successes
  type Result = (Int,Int)

  blueRoll, greenRoll, yellowRoll, purpleRoll :: Fractional prob => T prob Result
  blueRoll = norm $ uniform [(0,0),(0,0),(1,0),(1,1),(0,2),(0,1)]
  greenRoll = norm $ uniform [(0,0),(1,0),(1,0),(2,0),(0,1),(0,1),(1,1),(0,2)]
  yellowRoll = norm $ uniform [(0,0),(1,0),(1,0),(2,0),(2,0),(0,1),(1,1),(1,1),(1,1),(0,2),(0,2),(1,0)]
  purpleRoll = norm $ uniform [(0,0),(-1,0),(-2,0),(0,-1),(0,-1),(0,-1),(0,-2),(-1,-1)]
  
  pairPlus :: (Num n) => (n,n) -> (n,n) -> (n,n)
  pairPlus (x1,x2) (y1,y2) = (x1+y1,x2+y2)
  pairMSSum :: (Num n) => (MultiSet (n,n)) -> (n,n)
  pairMSSum = MS.fold pairPlus (0,0)
 
  roll :: Fractional prob => Int -> Int -> Int -> Int -> T prob Result
  roll g y u p = do
      gs <- norm $ independentRepFold pairPlus go g greenRoll
      ys <- norm $ independentRepFold pairPlus go y yellowRoll
      ps <- norm $ independentRepFold pairPlus go p purpleRoll
      bs <- norm $ independentRepFold pairPlus go u blueRoll
      return $ foldr pairPlus (0,0) [gs,ys,ps,bs]
    where go n (x,y) = (n*x,n*y)

  rollSucc :: Fractional prob => Int -> Int -> Int -> Int -> T prob Int
  rollSucc g y u p = do
      gs <- norm $ independentRepFold (+) (\n (x,_) -> n*x) g greenRoll
      ys <- norm $ independentRepFold (+) (\n (x,_) -> n*x) y yellowRoll
      ps <- norm $ independentRepFold (+) (\n (x,_) -> n*x) p purpleRoll
      bs <- norm $ independentRepFold (+) (\n (x,_) -> n*x) u blueRoll
      return $ sum [gs,ys,ps,bs]


  blasterRifleDmg g y u p = do
       (s,_) <- roll g y u p
       return $ if s > 0
                then s + 9
                else 0

  heavyBlasterDmg g y u p =  do
       (s,_) <- roll g y u p
       return $ if s > 0
                then s + 10
                else 0

  heavyBlAutoDmg g y u p = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s + 10) * (1+ floor ((max a 0) % 2))
                else 0

  heavyBlAutoJRDmg g y u p = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s + 10) * (1+ (max a 0))
                else 0
  
  heavlyBlpistol g y u p = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s+7)
                else 0

  equalizerDmg g y u p = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s+8)
                else 0

  aspDmg g y u p soak = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s + 4 - soak) * (1+ floor ((max a 0) % 2))
                else 0

  aspJRDmg g y u p soak = do
       (s,a) <- roll g y u (p+1)
       return $ if s > 0
                then (s + 4 - soak) * (1+ (max a 0))
                else 0
