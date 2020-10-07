solve :: [Integer] -> [Char]
solve (a:b:rest)
    | 2^b >= a = "Your wish is granted!"
    | otherwise = "You will become a flying monkey!"

main = do
    is <- getLine
    let ns = map read (words is) :: [Integer]
    let o = solve ns
    putStrLn o