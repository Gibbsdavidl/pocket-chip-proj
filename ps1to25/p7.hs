import System.Environment

-- problem 7

listify n x l
  | x == [] = filter (\y -> length y > 4) l
  | otherwise = listify n (drop 1 x) ((take n x):l) 


findp x = listify 5 x []


main = do
  args <- getArgs
  putStrLn $ show $ findp (last args)

