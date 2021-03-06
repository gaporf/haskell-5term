module Integral
  (
    integrate
  ) where
    
    
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.List (foldl')
import System.Random (newStdGen, randomRs)
import System.IO.Unsafe (unsafePerformIO)

helperIO
  :: (Double -> Double)
  -> Double
  -> Double
  -> Int
  -> IO Double
helperIO f a b n = do
  g <- newStdGen
  let randomList = take n $ randomRs (a, b) g
  return $ (foldl' (+) 0 $ map (\x -> f x) randomList) / (fromIntegral n) * (b - a)

helper
  :: (Double -> Double)
  -> Double
  -> Double
  -> Int
  -> Double
helper f a b n = unsafePerformIO $ helperIO f a b n
  
helper2
  :: (Double -> Double)
  -> Double
  -> Double
  -> Int
  -> Int
  -> Double
helper2 f a b 1 m = helper f a b m
helper2 f a b n m = runEval $ do
  r1 <- rpar $ helper f a b m
  r2 <- rpar $ helper2 f a b (n - 1) m
  return $ r1 + r2

-- | Takes a function (R -> R), range [a, b], number of sparks and returns the integral
-- counted with using Monte Carlo method
integrate
  :: (Double -> Double)
  -> Double
  -> Double
  -> Int
  -> Double
integrate f a b 1 = helper f a b 3000000
integrate f a b n = (helper2 f a b n (div 3000000 n)) / (fromIntegral n)
