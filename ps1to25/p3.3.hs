
-- problem 3

isPrime :: Integer -> Integer -> Bool
isPrime x n
  | x == 2 = True
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

s :: Integer -> Integer -> Integer -> Integer
s y x r
  | (isDiv y x) && (isPrime x (oddDiv x)) =  s (div y x) (x+1) x
  | x < y = s y (x+1) r
  | otherwise = r

main = putStrLn $ show $ s 600851475143 2 1
