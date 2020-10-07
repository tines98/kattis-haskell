rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (a:as) x | x == a = as
              | otherwise = a : rem1 as x

diff :: Eq a => [a] -> [a] -> [a]
diff [] [] = []
diff as (x:xs) = diff (map rem1 as x) xs