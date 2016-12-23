
-- problem 3




s x =
    | (isDiv x 600851475143) && (isPrime x) = x
    | otherwise = s (x-2)

main = putStrLn $ show $ s $ ceiling (600851475143/2)
