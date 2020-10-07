import Data.List

bruh3 ns ms = (length (ns++ms)) - (length (nub (ns++ms)))

bruh2 :: Int -> Int -> [Int] -> Int
bruh2 n m xs = bruh3 (slice 0 (n-1) xs) (slice n (n+m-1) xs)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

bruh :: [String] -> Int
bruh (x:xs) = bruh2 (read [head x] :: Int) (read [last x] :: Int) (map read (init xs) :: [Int])

main = interact (show . bruh . lines)
