{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathHammer where
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Numeric.Probability.Distribution as D
  import Data.List
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Control.Arrow
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

  msReplicate :: (Ord a) => Int -> a -> MultiSet a
  msReplicate i a = MS.fromList $ replicate i a
  
  msFoldOccurM :: (Ord a, Monad m) => (a -> MS.Occur -> b -> m b) -> b -> MultiSet a -> m b
  msFoldOccurM f z ms = foldrM (uncurry f) z (MS.toOccurList ms)

  msFoldM :: (Ord a, Monad m) => (a -> b -> m b) -> b -> MultiSet a -> m b
  msFoldM f z ms = foldrM f z (MS.toList ms)


  
  mapMMap :: (Ord k, Monad m) => (k -> a -> m b) -> Map k a -> m (Map k b)
  mapMMap f m = go (Map.toList m)
      where go [] = return Map.empty
            go ((k,v):kvs) = do ma <- go kvs
                                v' <- f k v
                                return (Map.insert k v' ma)


  -- Repeated independent trials can be handled specially
  independent :: forall a. (Ord a) => Int -> T Frac a -> T Frac (MultiSet a)
  independent i d = fromFreqs . map go $ resultCounts
       where resultCounts = fixedSumCombos i (length . extract $ d)
             ds = decons d
             go :: [Int] -> (MultiSet a, Frac)
             go os = (MS.unions $ zipWith (\(e,_) o -> msReplicate o e) ds os,
                      (fromIntegral $ multinom os)*(product $ zipWith (\(_,ep) o -> power ep o) ds os))

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

  data AV = AV10 | AV11 | AV12 | AV13 | AV14 deriving (Eq, Show, Ord)
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
  data Strength = STR1 | STR2 | STR3 | STR4 | STR5 | STR6 | STR7 | STR8 | STR9 | STR10 
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

  tervigon1 :: T Frac (Int,Bool)
  tervigon1 = norm $ do rs <- independent 3 d6
                        return (MS.fold (+) 0 rs, MS.distinctSize rs == 3)

  tervigon :: Int -> T Frac Int
  tervigon 0 = uniform [0]
  tervigon n = do (s,b) <- tervigon1
                  if b
                      then do r <- norm $ tervigon (n-1)
                              return (r+s)
                      else return s

  -- #rolls -> DC -> stuff
  wod :: Int -> Int -> T Frac Int
  wod n dc = do x <- independent n (norm $ fmap go $ uniform [1..10])
                return $ max 0 (sum $ MS.toList x)
     where go 1 = -1
           go n = if n >= dc
                     then 1
                     else 0
