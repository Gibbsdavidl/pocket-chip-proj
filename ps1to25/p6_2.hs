import System.Environment

-- problem 6

checkn:: Integer -> [Integer] -> Bool
checkn x ps
  | ps == [] = True
  | mod x (head ps) == 0 = False
  | otherwise = checkn x (tail ps)

findp':: Integer -> Integer -> [Integer] -> Integer
findp' n x ps
  | n < 1 = last ps
  | checkn x ps = findp' (n-1) (x+2) (ps++[x])
  | otherwise = findp' n (x+2) ps

findp:: Integer -> Integer
findp n = findp' (n-4) 9 [3,5,7]

main = do
  args <- getArgs
  let n = read (last args) :: Integer
  putStrLn $ show $ findp n

