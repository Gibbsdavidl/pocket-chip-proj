
-- problem 3

isPrime :: Integer -> Integer -> Bool
isPrime x n
  | mod x n == 0 = False
  | n > 3 = isPrime x (n-2)
  | mod x 2 == 0 = False
  | otherwise = True

isDiv :: Integer -> Integer -> Bool
isDiv a b = mod a b == 0

makeOdd :: Integer -> Integer
makeOdd x
  | even x = x+1
  | otherwise = x

oddDiv :: Integer -> Integer
oddDiv x
  | even x = makeOdd $ (quot x 2) - 1
  | otherwise = makeOdd $ (quot x 2) + 1

s :: Integer -> Integer
s x
  | (isDiv 60085174 x) && (isPrime x (oddDiv x)) = x
  | x > 1 = s (x-2)
  | otherwise = 0

main = putStrLn $ show $ s $ (oddDiv 60085174)
