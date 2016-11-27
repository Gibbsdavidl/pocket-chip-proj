
-- problem 2

fib a b c
  | b > 4000000 = c
  | mod b 2 == 0 = fib b (a+b) (c+b)
  | otherwise = fib b (a+b) c

main = putStrLn $ show $ fib 1 2 0
