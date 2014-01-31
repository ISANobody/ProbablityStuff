{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathHammer where
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Numeric.Probability.Distribution as D
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Data.Ratio
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Util

  type Frac = Ratio Integer

  dmin :: (Fractional prob, Ord a) => T prob a -> T prob a -> T prob a
  dmin d1 d2 = do r1 <- d1
                  r2 <- d2
                  return $ min r1 r2

  dmax :: (Fractional prob, Ord a) => T prob a -> T prob a -> T prob a
  dmax d1 d2 = do r1 <- d1
                  r2 <- d2
                  return $ max r1 r2
  
  -- Find all lists from [0..i]^l that sum to i
  fixedSumCombos :: (Num n, Enum n) => n -> Int -> [[n]]
  fixedSumCombos i 1 = [[i]]
  fixedSumCombos i l = do x <- [0..i]
                          xs <- fixedSumCombos (i-x) (l-1)
                          return (x:xs)

  -- Create a 'singleton' MultiSet containing a single element with a
  -- specified number of repetitions
  msPolyton :: (Ord a) => MS.Occur -> a -> MultiSet a
  msPolyton i a = MS.insertMany a i $ MS.empty
  
  msFoldOccurM :: (Ord a, Monad m) => (a -> MS.Occur -> b -> m b) -> b -> MultiSet a -> m b
  msFoldOccurM f z ms = foldrM (uncurry f) z (MS.toOccurList ms)

  -- Repeated independent trials can be handled specially
  -- An individual outcome [x1,x2,...,xn] will occur with
  -- probability the multinomial times each of the 
  -- individual probabilities (i.e., pk^xk)
  -- In the most general form we leave the mapping from
  -- result vector to event parametric
  independentGen :: forall prob a b . Fractional prob => ([(a,Int)] -> b)
                 -> Int -> T prob a -> T prob b
  independentGen f i d = fromFreqs . fmap go  $ fixedSumCombos i (size d)
     where probs = map snd . decons $ d
           go :: [Int] -> (b,prob)
           go ns = (f $ zip (extract d) ns
                   ,(fromIntegral $ multinom ns)*(product $ zipWith power probs ns))

  -- Takes a replication function and a combining function
  independentRepFold :: Fractional prob => (b -> b -> b) -> (Int -> a -> b)
                     -> Int -> T prob a -> T prob b
  independentRepFold c r = independentGen (foldr1 c . map (\(e,o) -> r o e))

  independent :: (Fractional prob, Ord a) 
              => Int -> T prob a -> T prob (MultiSet a)
  independent = independentRepFold MS.union msPolyton

  -- Remove Nothing from a Multiset Maybe a, then rm the wrapper
  stripMaybe :: (Ord a) => MultiSet (Maybe a) -> MultiSet a
  stripMaybe = MS.mapMaybe id

  foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
  foldrM _ b []     = return b
  foldrM f b (x:xs) = do rs <- foldrM f b xs
                         f x rs
                         

  d6 :: (Fractional prob) => T prob Int
  d6 = uniform [1..6]

  d3 :: (Fractional prob) => T prob Int
  d3 = uniform [1..3]

  _3d6 :: (Fractional prob) => T prob (MultiSet Int)
  _3d6 = norm $ do r1 <- d6
                   r2 <- d6
                   r3 <- d6
                   return . MS.insert r1 . MS.insert r2 $ MS.singleton r3

  msSum :: (Num n) => (MultiSet n) -> n
  msSum = MS.fold (+) 0

  d6rending :: (Fractional prob) => T prob Int
  d6rending = do
        x <- uniform [1..6]
        if x == 6
        then uniform [7..9]
        else return x

  data AP = AP1 | AP2 | AP3 | AP4 | AP5 | AP6 | APDash deriving (Eq, Show, Ord)
  data SV = SV2 | SV3 | SV4 | SV5 | SV6 | SVDash deriving (Eq, Show, Ord)

  svToRoll :: SV -> Roll
  svToRoll SV2 = D2Plus
  svToRoll SV3 = D3Plus
  svToRoll SV4 = D4Plus
  svToRoll SV5 = D5Plus
  svToRoll SV6 = D6Plus
  svToRoll SVDash = Never

  negates :: AP -> SV -> Bool
  negates APDash _   = False
  negates AP1    _   = True
  negates AP2    _   = True
  negates AP3    SV2 = False
  negates AP3    _   = True
  negates AP4    SV2 = False
  negates AP4    SV3 = False
  negates AP4    _   = True
  negates AP5    SV2 = False
  negates AP5    SV3 = False
  negates AP5    SV4 = False
  negates AP5    _   = True
  negates AP6    SV2 = False
  negates AP6    SV3 = False
  negates AP6    SV4 = False
  negates AP6    SV5 = False
  negates AP6    _   = True

  data ShootingProfile = Heavy Int 
                       | Assault Int 
                       | RapidFire
                       | Ordinance 
                       | Multi [ShootingProfile] deriving (Show, Eq, Ord)

  data AV = AV10 | AV11 | AV12 | AV13 | AV14 deriving (Eq, Show, Ord, Enum)
  data CrewStatus = CrewFine | CrewShaken | CrewStunned
       deriving (Show, Eq, Ord)
  data VehicleDamage = VehicleDamage { crew :: CrewStatus, 
                                       weaponsDestroyed :: Int, 
                                       immobilized :: Bool, 
                                       hullLost :: Int }
                     | Wrecked
                     | Explodes
       deriving (Show, Eq, Ord)
  undamaged :: VehicleDamage
  undamaged = VehicleDamage { crew = CrewFine, 
                              weaponsDestroyed = 0,
                              immobilized = False, 
                              hullLost = 0 }
  killed :: VehicleDamage -> Bool
  killed Wrecked = True
  killed Explodes = True
  killed _ = False
  
  -- Note BS6+ needs to be handled specially
  data Strength = STR0 | STR1 | STR2 | STR3 | STR4 | STR5 | STR6 | STR7 | STR8 | STR9 | STR10 
       deriving (Eq, Show, Ord, Enum)

  data Toughness = T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10
       deriving (Eq, Show, Ord, Enum)

  data BS = BS0 | BS1 | BS2 | BS3 | BS4 | BS5 deriving (Eq, Show, Ord, Enum)

  data WS = WS0 | WS1 | WS2 | WS3 | WS4 | WS5 deriving (Eq, Show, Ord, Enum)

  data Roll = Always | D2Plus | D3Plus | D4Plus | D5Plus | D6Plus | Never deriving (Show, Eq)
  instance Enum Roll where
     fromEnum Always = 1
     fromEnum D2Plus = 2
     fromEnum D3Plus = 3
     fromEnum D4Plus = 4
     fromEnum D5Plus = 5
     fromEnum D6Plus = 6
     fromEnum Never  = 7
     toEnum 1 = Always
     toEnum 2 = D2Plus
     toEnum 3 = D3Plus
     toEnum 4 = D4Plus
     toEnum 5 = D5Plus
     toEnum 6 = D6Plus
     toEnum 7 = Never
     toEnum _ = error "Roll out of range"

  roll :: (Fractional prob) => Roll -> T prob Bool
  roll r = D.map (\ x -> x >= fromEnum r) d6

  -- I hate twinlinked.
  -- Take a twinlinked flag
  bs2prob :: (Fractional prob) => BS -> Bool -> T prob Bool
  bs2prob bs tl = norm $ do r <- d6
                            if r >= (7 - fromEnum bs)
                            then return True
                            else if tl
                                 then bs2prob bs False
                                 else return False

  -- Takes a BS, number of shots, and Twin-linked Flag. Returns the number of hits
  numhits :: (Fractional prob) => BS -> Int -> Bool -> T prob Int
  numhits b n tl = norm $ D.map (MS.occur True) $ norm $ independent n (bs2prob b tl)

  data PenResult = Failure | Glance | Pen deriving (Show, Eq, Ord, Enum)

  hit2pen :: (Fractional prob) => Strength -> AV -> T prob PenResult
  hit2pen s a = norm $ do r <- d6
                          return (case () of _ | r + str > av  -> Pen
                                               | r + str == av -> Glance
                                               | otherwise     -> Failure)
    where str = fromEnum s
          av  = 10 + fromEnum a

  pendmg :: (Fractional prob) => T prob Int
  pendmg = norm $ uniform [1,1,1,1,2,3]

  quadshooting :: (Fractional prob) => BS -> T prob Bool
  quadshooting bs = do h <- numhits bs 4 True
                       r <- independent h (norm $ hit2pen STR7 AV10)
                       d <- independent (MS.occur Pen r) pendmg
                       return $ MS.occur Glance r + msSum d >= 3

  crimsonHit :: (Fractional prob) => AV -> T prob PenResult
  crimsonHit av = norm $ do r <- d6
                            case () of 
                             _ | r+8 == 10 + fromEnum av -> return Glance
                               | r+8 > 10 + fromEnum av  -> return Pen
                               | otherwise -> hit2pen STR8 av

  ap2VSFlyerDmg :: (Fractional prob) => Int -> T prob Int
  ap2VSFlyerDmg hp = norm $ uniform [1,1,1,2,hp,hp]

  crimsonVSturkey :: (Ord prob, Fractional prob) => T prob Bool
  crimsonVSturkey = do h <- numhits BS5 4 False
                       h'<- fmap (MS.occur True) $ independent h (fmap (<5) d6)
                       r <- independent h' (crimsonHit AV12)
                       d <- independent (MS.occur Pen r) (ap2VSFlyerDmg 3)
                       return $ MS.occur Glance r + msSum d >= 3

  vendettaVSturkey :: (Fractional prob) => T prob Bool
  vendettaVSturkey = do h <- numhits BS3 3 True
                        h'<- fmap (MS.occur True) $ independent h (fmap (<5) d6)
                        r <- independent h' (hit2pen STR9 AV12)
                        d <- independent (MS.occur Pen r) (ap2VSFlyerDmg 3)
                        return $ MS.occur Glance r + msSum d >= 3

  quadgunVSturkey :: (Fractional prob) => T prob Bool
  quadgunVSturkey = do h <- numhits BS4 4 True
                       h'<- fmap (MS.occur True) $ independent h (fmap (<5) d6)
                       r <- independent h' (hit2pen STR7 AV12)
                       d <- independent (MS.occur Pen r) pendmg
                       return $ MS.occur Glance r + msSum d >= 3

  lootasVSturkey :: (Fractional prob) => T prob Bool
  lootasVSturkey = do h <- norm $ numhits BS1 20 False
                      h'<- fmap (MS.occur True) $ norm $ independent h (fmap (<5) d6)
                      r <- independent h' (hit2pen STR7 AV12)
                      d <- independent (MS.occur Pen r) pendmg
                      return $ MS.occur Glance r + msSum d >= 3
