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
 
  succsplit :: Fractional prob => T prob Result -> (T prob Int, T prob Result, T prob Result)
  succsplit d = (Cons [(1,go1 ?? d),(0,go2 ?? d)], go1 ?=<< d, go2 ?=<< d)
    where go1 = (\(x,_) -> x /= 0)
          go2 = (\(x,_) -> x == 0)
  
  pairPlus :: (Num n) => (n,n) -> (n,n) -> (n,n)
  pairPlus (x1,x2) (y1,y2) = (x1+y1,x2+y2)
  pairMSSum :: (Num n) => (MultiSet (n,n)) -> (n,n)
  pairMSSum = MS.fold pairPlus (0,0)
 
  -- Pretty slow, particularly for inputs like 2 4 4 4. Might want a more specialized
  -- query if these problems show up
  roll :: Fractional prob => Int -> Int -> Int -> Int -> T prob Result
  roll g y u p = do
      gs <- norm $ independentRepFold pairPlus go g greenRoll
      ys <- norm $ independentRepFold pairPlus go y yellowRoll
      ps <- norm $ independentRepFold pairPlus go p purpleRoll
      bs <- norm $ independentRepFold pairPlus go u blueRoll
      return $ foldr pairPlus (0,0) [gs,ys,ps,bs]
    where go n (x,y) = (n*x,n*y)

  -- Only look for successes
  rollSuccOnly :: Fractional prob => Int -> Int -> Int -> Int -> T prob Int
  rollSuccOnly g y u p = do
      bs <- norm $ independentRepFold (+) (\n (x,_) -> n*x) u blueRoll
      gs <- norm $ independentRepFold (+) (\n (x,_) -> n*x) g greenRoll
      ys <- norm $ independentRepFold (+) (\n (x,_) -> n*x) y yellowRoll
      ps <- norm $ independentRepFold (+) (\n (x,_) -> n*x) p purpleRoll
      return $ sum [gs,ys,ps,bs]

  -- Track Both but only for successful roll using Maybe
  -- Short circuits, so should be faster, but isn't
  rollSucc :: Fractional prob => Int -> Int -> Int -> Int -> T prob (Maybe Result)
  rollSucc g y u p = do
     bsrolls <- norm $ independentRepFold (+) (*) u bc
     gsrolls <- norm $ independentRepFold (+) (*) g gc
     psrolls <- norm $ independentRepFold (+) (*) p pc
     (bsuc,bad1) <- norm $ independentRepFold pairPlus go bsrolls bs
     (gsuc,gad1) <- norm $ independentRepFold pairPlus go gsrolls gs
     (psuc,pad1) <- norm $ independentRepFold pairPlus go psrolls ps
     if bsuc+gsuc+psuc > 0
     then do (_,bad2) <- norm $ independentRepFold pairPlus go (u-bsrolls) bf
             (_,gad2) <- norm $ independentRepFold pairPlus go (g-gsrolls) gf
             (_,pad2) <- norm $ independentRepFold pairPlus go (p-psrolls) pf
             return $ Just (bsuc+gsuc+psuc,bad1+bad2+gad1+gad2+pad1+pad2)
     else return Nothing
    where go n (x,z) = (n*x,n*z)
          (bc,bs,bf) = succsplit blueRoll
          (gc,gs,gf) = succsplit greenRoll
          (pc,ps,pf) = succsplit purpleRoll

  regular :: Fractional prob => Int -> Int -> Int -> Int -> T prob Int
  regular g y u p = do (s,a) <- roll g y u p
                       if s > 0
                       then return $ 9+s
                       else return $ 0

  duelWield :: Fractional prob => Int -> Int -> Int -> Int -> T prob Int
  duelWield g y u p = do (s,a) <- roll g y u (p+1)
                         case () of
                          _ | s > 0 && a >= 0 -> return $ 2 * (9+s)
                            | s > 0 -> return $ 9+s
                            | otherwise -> return 0

  autoFire :: Fractional prob => Int -> Int -> Int -> Int -> T prob Int
  autoFire g y u p = do (s,a) <- roll g y u (p+1)
                        if s > 0
                        then return $ (10+s) * (1 + max 0 a)
                        else return 0
