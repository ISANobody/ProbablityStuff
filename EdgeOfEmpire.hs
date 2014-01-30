{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeOfEmpire where
  import MathHammer hiding (roll)
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Data.Ratio

  -- Fixes some type class generality
  myExpected :: (Fractional p, Integral n) => T p n -> p
  myExpected = expected . (fmap fromIntegral)

  -- Bad name (Success,Advantage)
  -- Treat Triumphs as successes
  type Result = (Int,Int)

  blueRoll, greenRoll, yellowRoll, purpleRoll :: Fractional prob => T prob Result
  blueRoll = uniform [(0,0),(0,0),(1,0),(1,1),(0,2),(0,1)]
  greenRoll = uniform [(0,0),(1,0),(1,0),(2,0),(0,1),(0,1),(1,1),(0,2)]
  yellowRoll = uniform [(0,0),(1,0),(1,0),(2,0),(2,0),(0,1),(1,1),(1,1),(1,1),(0,2),(0,2),(1,0)]
  purpleRoll = uniform [(0,0),(-1,0),(-2,0),(0,-1),(0,-1),(0,-1),(0,-2),(-1,-1)]
  
  pairPlus :: (Num n) => (n,n) -> (n,n) -> (n,n)
  pairPlus (x1,x2) (y1,y2) = (x1+y1,x2+y2)
  pairMSSum :: (Num n) => (MultiSet (n,n)) -> (n,n)
  pairMSSum = MS.fold pairPlus (0,0)
 
  roll :: Fractional prob => Int -> Int -> Int -> Int -> T prob Result
  roll g y u p = do
      gs <- norm $ independent g greenRoll
      ys <- norm $ independent y yellowRoll
      ps <- norm $ independent p purpleRoll
      bs <- norm $ independent u blueRoll
      return $ foldr pairPlus (0,0) (map pairMSSum [gs,ys,ps,bs])


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
