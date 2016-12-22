
-- problem 6

r x = (length x, last x)

checkn' a n
  | a <= n      = 1
  | mod a n==0 = 0
  | otherwise = checkn' a (n+2)

checkn a
  | checkn' a 3 == 1 = a
  | otherwise = 0

main = putStrLn $ show $ r $ filter (> 0) $ map checkn [9,11..99999]
