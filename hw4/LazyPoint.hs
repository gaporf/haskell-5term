module LazyPoint
  (
    LazyPoint (LazyPoint)
  , perimeter
  , doubleArea
  ) where
      
data LazyPoint = LazyPoint Int Int
  deriving (Eq, Show)
  
plus
  :: LazyPoint
  -> LazyPoint
  -> LazyPoint
plus (LazyPoint x1 y1) (LazyPoint x2 y2) = LazyPoint (x1 + x2) (y1 + y2)

minus
  :: LazyPoint
  -> LazyPoint
  -> LazyPoint
minus p1 (LazyPoint x y) = plus p1 (LazyPoint (-x) (-y))

scalarProduct
  :: LazyPoint
  -> LazyPoint
  -> Int
scalarProduct (LazyPoint x1 y1) (LazyPoint x2 y2) = x1 * x2 + y1 * y2

crossProduct
  :: LazyPoint
  -> LazyPoint
  -> Int
crossProduct p1 (LazyPoint x y) = scalarProduct p1 (LazyPoint y (-x))

shift
  :: [LazyPoint]
  -> [LazyPoint]
shift (x : xs) = (xs ++ [x])

helper
  :: (LazyPoint, LazyPoint)
  -> Double
  -> Double
helper (a, b) sum = sum + (sqrt $ fromIntegral $ scalarProduct (minus a b) (minus a b))

perimeter
  :: [LazyPoint]
  -> Double
perimeter [] = 0
perimeter pts = foldr helper 0 (zip pts $ shift pts)

helper2
  :: (LazyPoint, LazyPoint)
  -> Int
  -> Int
helper2 ((LazyPoint x1 y1), (LazyPoint x2 y2)) sum = sum + (x1 - x2) * (y1 + y2)

doubleArea
  :: [LazyPoint]
  -> Int
doubleArea []  = 0
doubleArea pts = foldr helper2 0 (zip pts $ shift pts)
