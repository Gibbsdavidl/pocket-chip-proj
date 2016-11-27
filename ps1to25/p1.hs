
-- problrm 1

checkn a
  | mod a 3==0 || mod a 5==0 = a
  | otherwise = 0

main = putStrLn $ show $ sum $ map checkn [1..999]
