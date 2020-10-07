sumsq :: Int -> Int
sumsq n = foldr (+) 0 (sumsq' n)

sumsq' :: Int -> [Int]
sumsq' 0 = []
sumsq' n = n^2:sumsq' (n-1)

lengtho :: [t] -> Int
lengtho xs = foldl (\x y -> x+1) 0 xs

minlist :: [Int] -> Int
minlist xs = foldl1 (\x y -> min x y) xs

reverso :: [t] -> [t]
reverso xs = foldr (\x y -> y++[x]) [] xs

removo :: [Char] -> [Char] -> [Char]
removo xs ys = foldr (\x y -> if elem x y then ([x]++y) else (""++y)) xs ys 

remdups :: (Eq t) => [t] -> [t]
remdups [] = []
remdups xs = foldr remdup [] xs

remdup :: (Eq t) => t -> [t] -> [t]
remdup x [] = [x]
remdup x ys = if x == head ys then ys else x:ys  

filteror :: (a -> Bool) -> [a] -> [a]
filteror pr xs = foldr (\x ys -> if pr x then x:ys else ys) [] xs

filterol :: (a -> Bool) -> [a] -> [a]
filterol pr xs = foldl (\ys x -> if pr x then ys++[x] else ys) [] xs

initsio :: Eq t => [t] -> [[t]]
initsio xs = foldl (\ys x -> if ys == [[]] then ys ++ [[x]] else ys ++[(last ys) ++ [x]]) [[]] xs

approxe :: (Eq t, Fractional t) => t -> t
approxe 0 = facul 1
approxe n = ((/) 1 (facul n)) + approxe (n-1)

facul :: (Eq t, Num t) => t -> t
facul 0 = 1
facul n = n * facul (n-1)