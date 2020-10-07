bruh :: Int -> IO()
bruh 0 = return ()
bruh n = do
    getLine
    bruh (n-1)

main:: IO ()
main = do
    i <- getLine
    let input = map read (words i) :: [Int]
    bruh (head input)
    print $ (head (reverse input))