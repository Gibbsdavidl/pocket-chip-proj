import System.Environment
import Data.List

-- problem 8

-- a < b < c
-- a^2 + b^2 == c^2
-- a+b+c == 1000

t1 a b = (a < b) && (b < (1000-a-b))

t2 a b = (a^2 + b^2) == ((1000-a-b)^2)

t3 a b = (a+b) < 1000

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

aseq n = take n [999,998..]

findp (a,b) = ( ((t1 a b) && (t2 a b) && (t3 a b)) , a, b)

main = do
  args <- getArgs
  putStrLn $
    show $
      filter (\(x,y,z) -> x == True) $
        map findp (cartProd (aseq 900) (aseq 900))
