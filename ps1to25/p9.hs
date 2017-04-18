import System.Environment
import Data.List
import Data.Fixed

-- problem 9

-- sum of primes less than 2mil

divides :: Double -> Double -> Bool
divides x y
  | (mod' x y) == 0 = True -- not a prime
  | otherwise = False   -- might be


isaPrime :: Double -> Bool -> [Double] -> Bool
isaPrime n b [] = b
isaPrime n b ps
  | divides n (head ps) = False -- not a prime
  | otherwise = isaPrime n b (tail ps)


primes' :: Double -> [Double] -> Double -> [Double]
primes' maxVal ps n
  -- if n > maxVal return ps
  | n > maxVal = ps
  -- test if n is a primes
  | isaPrime n True ps = primes' maxVal (ps++[n]) (n+2)
  -- otherwise move one, add 2 to n
  | otherwise = primes' maxVal (ps) (n+2)


primes :: Double -> [Double]
primes maxVal = primes' maxVal [2] 3


main = do
  args <- getArgs
  let maxVal = (read (args !! 0) :: Double)
  putStrLn $ show $ sum $ primes maxVal
