{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MathHammer where
  import Numeric.Probability.Distribution hiding (map,filter)
  import qualified Numeric.Probability.Distribution as D
  import qualified Data.MultiSet as MS
  import Data.MultiSet (MultiSet)
  import Data.Ratio
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

  -- Split a MultiSet into two
  mssplits :: (Ord a) => MultiSet a -> [(MultiSet a,MultiSet a)]
  mssplits = msFoldOccurM go (MS.empty,MS.empty)
    where go x o (l,r) = do n <- [0..o]
                            return (MS.insertMany x n l,MS.insertMany x (o-n) r)

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

  -- N trials with replacement
  independent :: (Fractional prob, Ord a) 
              => Int -> T prob a -> T prob (MultiSet a)
  independent = independentRepFold MS.union msPolyton

  -- N trials with replacement each subtrial can produce a multiset too
  independentUnion :: forall prob a. (Fractional prob, Ord a) 
              => Int -> T prob (MultiSet a) -> T prob (MultiSet a)
  independentUnion = independentRepFold MS.union go
    where go :: Int -> MultiSet a -> MultiSet a
          go n = MS.fromMap . fmap (n*) . MS.toMap


  -- N trials without replacement
  -- If you need a reference look for multivariate hypergeometric distributions

  -- First calculate the possible outcomes (c.f. fixedSumCombos)
  -- The big difference is that each element has a maximum number of times it can
  -- occur. It probably isn't dangerous for there to be repeats in the input map.
  -- Efficiency guess: this should use Map a Int for its result, or at least use
  -- groupBy somewhere if you want only the Eq typeclass constraint
  combosWO :: [Int]       -- Usage caps for each element
           -> Int         -- Selection Size
           -> [[Int]]     -- Resulting outcomes
  combosWO caps   0 = [map (const 0) caps]
  combosWO []     _ = []
  combosWO (c:cs) k = do x <- [0..min c k]
                         r <- combosWO cs (k-x)
                         return (x:r)

  -- As above but take in multisets for capac
  combosWOMS :: (Ord a)
             => MultiSet a   -- Population
             -> Int          -- Selection Size
             -> [MultiSet a] -- List of possible selections
  combosWOMS ms k = map go (combosWO (map snd asc) k)
    where asc = MS.toAscOccurList ms
          go  = MS.fromAscOccurList . zipWith (\(a,_) c -> (a,c)) asc

  -- Multivariate Hypergeometric Distribution
  hypergeometric :: (Fractional prob, Ord a) 
                 => MultiSet a          -- Starting population
                 -> Int                 -- Selection Size
                 -> T prob (MultiSet a) -- Result distribution
  hypergeometric pop k = fromFreqs $ map go (combosWOMS pop k)
     where go o = (o,(MS.foldOccur (\a c acc -> acc * (binom (MS.occur a pop) c)) 1 o)
                     / (binom (MS.size pop) k))
                 


  -- Remove Nothing from a Multiset Maybe a, then rm the wrapper
  stripMaybe :: (Ord a) => MultiSet (Maybe a) -> MultiSet a
  stripMaybe = MS.mapMaybe id

  foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
  foldrM _ b []     = return b
  foldrM f b (x:xs) = do rs <- foldrM f b xs
                         f x rs

  nonZeroOccur :: (Ord a) => a -> MultiSet a -> Bool
  nonZeroOccur a m = MS.occur a m > 0

  data Color = U | B | G | R | W | C deriving (Eq,Show,Ord,Enum)
  data Card = Spell (MultiSet Color)
            | BasicLand Color
            | PainLand Color Color
            | FetchLand Color Color
            | TriLand Color Color Color
     deriving (Eq,Show,Ord)


  -- Entries for Carl's Cards
  -- Lands
  battlefieldForge, bloodstainedMire, cavesOfKoilos, mountain, nomadOutpost :: Card
  plains, pollutedDelta, swamp, urborgTombOfYawgmoth, windsweptHeath :: Card

  battlefieldForge     = PainLand R W
  windsweptHeath       = FetchLand W G
  urborgTombOfYawgmoth = BasicLand B -- Wrong
  swamp                = BasicLand B
  pollutedDelta        = FetchLand U B
  plains               = BasicLand W
  mountain             = BasicLand R
  nomadOutpost         = TriLand R W B
  cavesOfKoilos        = PainLand W B
  bloodstainedMire     = FetchLand R B


  -- Creatures
  athreosGodOfPassage, bloodsoakedChampion, butcherOfTheHorde, chiefOfTheEdge :: Card
  tormentedHero, goblinRabblemaster, purphorosGodOfTheForge, soldierOfThePantheon :: Card
  tymaretTheMurderKing :: Card

  athreosGodOfPassage    = Spell (MS.fromList [C,W,B])
  bloodsoakedChampion    = Spell (MS.singleton B)
  butcherOfTheHorde      = Spell (MS.fromList [C,R,W,B])
  chiefOfTheEdge         = Spell (MS.fromList [W,B])
  tormentedHero          = Spell (MS.singleton B)
  goblinRabblemaster     = Spell (MS.fromList [C,C,R])
  purphorosGodOfTheForge = Spell (MS.fromList [C,C,C,R])
  soldierOfThePantheon   = Spell (MS.singleton W)
  tymaretTheMurderKing   = Spell (MS.fromList [W,B])

  deck :: MultiSet Card
  deck = MS.fromList . concat $
         [replicate 2 battlefieldForge
         ,replicate 4 bloodstainedMire
         ,replicate 3 cavesOfKoilos
         ,replicate 3 mountain
         ,replicate 2 nomadOutpost
         ,replicate 2 plains
         ,replicate 1 pollutedDelta
         ,replicate 4 swamp
         ,replicate 1 urborgTombOfYawgmoth
         ,replicate 1 windsweptHeath
         ,replicate 2 athreosGodOfPassage
         ,replicate 4 bloodsoakedChampion
         ,replicate 2 butcherOfTheHorde
         ,replicate 4 chiefOfTheEdge
         ,replicate 4 tormentedHero
         ,replicate 4 goblinRabblemaster
         ,replicate 2 purphorosGodOfTheForge
         ,replicate 4 soldierOfThePantheon
         ,replicate 2 tymaretTheMurderKing]

  -- Since fetch lands can make this tricky we just track all the possibilities
  availableMana :: MultiSet Card -> [MultiSet Color]
  availableMana = MS.fold go [MS.empty]
    where go = undefined

  main :: IO ()
  main = print $ ((any (\m -> nonZeroOccur B m)) ?? (fmap availableMana (hypergeometric deck 1)) :: Double)

{-
//Lands
2 Battlefield Forge
4 Bloodstained Mire
3 Caves of Koilos
3 Mountain
2 Nomad Outpost
2 Plains
1 Polluted Delta
4 Swamp
1 Urborg, Tomb of Yawgmoth
1 Windswept Heath


//Spells
2 Crackling Doom
1 Hero's Downfall
1 Mardu Ascendancy
3 Mardu Charm
2 Ride Down

//Creatures
2 Athreos, God of Passage
4 Bloodsoaked Champion
2 Butcher of the Horde
4 Chief of the Edge
4 Tormented Hero
4 Goblin Rabblemaster - need 1 to replace phials loan
2 Purphoros, God of the Forge
4 Soldier of the Pantheon
2 Tymaret, the Murder King


Deckstats version
//Lands
2 Battlefield Forge
4 Bloodstained Mire
3 Caves of Koilos
3 Mountain
2 Nomad Outpost
2 Plains
1 Polluted Delta
4 Swamp
1 Urborg, Tomb of Yawgmoth
1 Windswept Heath

//Spells
2 Crackling Doom
1 Hero's Downfall
1 Mardu Ascendancy
3 Mardu Charm
2 Ride Down

//Creatures
2 Athreos, God of Passage
4 Bloodsoaked Champion
2 Butcher of the Horde
4 Chief of the Edge
4 Goblin Rabblemaster
2 Purphoros, God of the Forge
4 Soldier of the Pantheon
4 Tormented Hero
2 Tymaret, the Murder King
-}
