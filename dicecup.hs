solve :: [Int] -> [Int]
solve (a:b:rest) = [min a b + 1 .. max a b +1]


main :: IO ()
main = do
    is <- getLine
    let ns  = map read (words is) :: [Int]
    mapM_ print (solve ns) 
    