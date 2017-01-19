
-- problrm 6

pairNE (a,b) = a != b

pairProd (a,b) = a*b

cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sqNoDiag a = sum $ map pairProd (filter pairNE (cartProd a a))

main = putStrLn $ show $ sqNoDiag 100
