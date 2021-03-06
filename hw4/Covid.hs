module Covid
  (
    Grid
  , newGrid
  , readGrid
  , evolve
  ) where

import Control.Comonad (Comonad, duplicate, extend, extract)
import Data.List (intercalate)
import System.Random (newStdGen, randomRs)
import System.IO.Unsafe (unsafePerformIO)

data ListZipper a = LZ [a] a [a]

listLeft 
  :: ListZipper a 
  -> ListZipper a
listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft _                  = undefined

listRight 
  :: ListZipper a 
  -> ListZipper a
listRight (LZ a x (b : bs)) = LZ (x : a) b bs
listRight _                 = undefined

listWrite 
  :: a 
  -> ListZipper a 
  -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList 
  :: ListZipper a 
  -> Int 
  -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap f (LZ ls x rs)= LZ (map f ls) (f x) (map f rs)
  
extract 
  :: ListZipper a 
  -> a
extract (LZ _ x _) = x

iterateTail
  :: (a -> a)
  -> a
  -> [a]
iterateTail f = tail . iterate f

genericMove
  :: (z a -> z a)
  -> (z a -> z a)
  -> z a
  -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

duplicate :: ListZipper a -> ListZipper (ListZipper a)
duplicate = genericMove listLeft listRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up
  :: Grid a
  -> Grid a
up (Grid g) = Grid (listLeft g)

down
  :: Grid a
  -> Grid a
down (Grid g) = Grid (listRight g)

left
  :: Grid a
  -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right
  :: Grid a
  -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead
  :: Grid a
  -> a
gridRead (Grid g) = Covid.extract $ Covid.extract g

gridWrite
  :: a
  -> Grid a
  -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = Covid.extract g
    newLine = listWrite x oldLine
    
horizontal
  :: Grid a
  -> ListZipper (Grid a)
horizontal = genericMove left right

vertical
  :: Grid a
  -> ListZipper (Grid a)
vertical = genericMove up down

instance Functor Grid where
  fmap f (Grid g) = Grid (fmap (fmap f) g)
  
instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical
  
data Phase
  = Health
  | Incubation Int
  | Illness Int
  | Immunity Int
  
data Man = Man Phase Double Int Int Int

instance Show Man where
  show (Man Health _ _ _ _)         = " "
  show (Man (Incubation _) _ _ _ _) = "$"
  show (Man (Illness _) _ _ _ _)    = "#"
  show (Man (Immunity _) _ _ _ _)   = "@"
  
-- | takes the arguments p -- probability of contamination, inc -- the duration of incubation,
-- ill -- the duration of illness, im -- the duration of immunity 
newGrid 
  :: Double
  -> Int
  -> Int
  -> Int
  -> Grid Man
newGrid p inc ill imm = let h = (Man Health p inc ill imm) 
                        in let g = Grid $ Covid.duplicate (LZ (repeat h) h (repeat h))
                          in gridWrite (Man (Illness ill) p inc ill imm) g
             
helper
  :: Grid Man
  -> Int
  -> String
helper g n = (intercalate "" $ fmap show $ toList (Covid.extract (unGrid g)) n) ++ "\n"

helperUp
  :: Grid Man
  -> Int
  -> Int
  -> String
helperUp g n (-1) = ""
helperUp g n 0    = helper g n
helperUp g n m    = (helperUp (up g) n (m - 1)) ++ (helper g n)

helperDown
  :: Grid Man
  -> Int
  -> Int
  -> String
helperDown g n (-1) = ""
helperDown g n 0    = helper g n
helperDown g n m    = (helper g n) ++ (helperDown (down g) n (m - 1))

-- | takes the size of size of square n and returns square with side of (2 * n + 1)
readGrid
  :: Grid Man
  -> Int
  -> String
readGrid g n = (helperUp (up g) n (n - 1)) ++ (helper g n) ++ (helperDown (down g) n (n - 1))

isCont
  :: Man
  -> Bool
isCont (Man (Incubation _) _ _ _ _) = True
isCont (Man (Illness _) _ _ _ _)    = True
isCont _                            = False

contCount
  :: [Man]
  -> Int
contCount = length . filter (\man -> isCont man)

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals = [left, right]
    verticals = [up, down]
    
contNeighbours
  :: Grid Man
  -> Int
contNeighbours g = contCount $ map (\direction -> Control.Comonad.extract $ direction g) neighbours

randDouble :: IO Double
randDouble = do
  g <- newStdGen
  return $ head $ randomRs(0, 1) g
  
tryRand
  :: Double
  -> Int
  -> Bool
tryRand p n = (unsafePerformIO $ randDouble) > ((1 - p) ** (fromIntegral n))

tryCont
  :: Grid Man
  -> Double
  -> Int
  -> Int
  -> Int
  -> Man
tryCont g p inc ill imm = if tryRand p (contNeighbours g)
                            then (Man (Incubation inc) p inc ill imm)
                            else (Man Health p inc ill imm)
  
rule
  :: Grid Man
  -> Man
rule g = case (Control.Comonad.extract g) of
  (Man Health p inc ill imm)         -> tryCont g p inc ill imm
  (Man (Incubation 0) p inc ill imm) -> (Man (Illness ill) p inc ill imm)
  (Man (Incubation n) p inc ill imm) -> (Man (Incubation $ n - 1) p inc ill imm)
  (Man (Illness 0) p inc ill imm)    -> (Man (Immunity imm) p inc ill imm)
  (Man (Illness n) p inc ill imm)    -> (Man (Illness $ n - 1) p inc ill imm)
  (Man (Immunity 0) p inc ill imm)   -> (Man Health p inc ill imm)
  (Man (Immunity n) p inc ill imm)   -> (Man (Immunity $ n - 1) p inc ill imm)
  
evolve
  :: Grid Man
  -> Grid Man
evolve = extend rule
