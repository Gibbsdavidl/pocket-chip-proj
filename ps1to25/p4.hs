
-- problem 4

checkn n r t
  | r==1 = t
  | mod n r == 0 = checkn n (r-1) True
  | otherwise = False

div20 n 
  | n > 1000000000 = 1000000000
  | checkn n 19 False = n
  | otherwise = div20 (20+n)

main = putStrLn $ show $ div20 20
