one :: [t] -> [t]
one xs
    | length xs < 5 = error("bruh")
    | otherwise = one' xs 5

one' :: [t] -> Int -> [t]
one' xs 0 = []
one' (x:xs) n = x:(one' xs (n-1))

two :: (Num a) => [a] -> a
two [] = 0
two (x:xs) = x + two xs

three :: Int -> Int
three n = if n <= 1 then 1 else (three(n-1) + three(n-2))

-- four :: Int -> Int
-- four x = 