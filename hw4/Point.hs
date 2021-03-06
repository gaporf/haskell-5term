{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module Point
  (
    Point (Point)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  ) where
    
import Control.DeepSeq (NFData, rnf, ($!!))
    
-- | Point representation. Constructor takes two Int arguments
data Point = Point !Int !Int
  deriving (Eq, Show)

instance NFData Point where
  rnf
   :: Point
   -> ()
  rnf (Point x y) = rnf x `seq` rnf y
  
-- | Takes two Points and returns new Point with sum of corresponding coordinates
plus
  :: Point
  -> Point
  -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Takes two Points and returns new Point with sub of corresponding coordinates
minus
  :: Point
  -> Point
  -> Point
minus p1 (Point x y) = plus p1 (Point (-x) (-y))

-- | Takes two Points and returns the Int scalar product of corresponding coordinates 
scalarProduct
  :: Point
  -> Point
  -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Takes two Points and return the Int cross product of corresponding coordinates
crossProduct
  :: Point
  -> Point
  -> Int
crossProduct p1 (Point x y) = scalarProduct p1 (Point y (-x))

helper
  :: Point
  -> Point
  -> Double
helper p1 p2 = sqrt $ fromIntegral $ scalarProduct p p
  where
    p = minus p1 p2
  
-- | Takes the list of Points and returns the perimeter of polygon in counter-clockwise order
perimeter
  :: [Point]
  -> Double
perimeter []           = 0
perimeter pts@(x : xs) = go 0 x pts
  where
    go !acc f (l : [])          = (acc +) $!! (helper l f)
    go !acc f (a : pts@(b : _)) = go ((acc +) $!! (helper a b)) f pts
    
helper2
  :: Point
  -> Point
  -> Int
helper2 p1@(Point x1 y1) p2@(Point x2 y2) = (crossProduct p1 p2) + (scalarProduct p1' p2')
  where
    p1' = (Point x1 (-x2))
    p2' = (Point y1 y2)

-- | Takes the list of Points and returns the double area of polygon in counter-clockwise order
doubleArea
  :: [Point]
  -> Int
doubleArea []           = 0
doubleArea pts@(x : xs) = go 0 x pts
  where
    go !acc f (l : [])          = (acc +) $!! (helper2 l f)
    go !acc f (a : pts@(b : _)) = go ((acc +) $!! (helper2 a b)) f pts
