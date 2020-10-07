main = do
    as <- getLine
    let xs = map read (words as) :: [Int]
    let a = div (sum xs) 2
    return a