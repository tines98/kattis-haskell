solve :: Int -> Int -> Int
solve bottles c = if bottles < c then 0 else 1 + solve (bottles-c+1) c

main :: IO ()
main = do
    is <- getLine
    let ns  = map read (words is) :: [Int]
    print $ solve (sum(tail (reverse ns))) (head (reverse ns))
    