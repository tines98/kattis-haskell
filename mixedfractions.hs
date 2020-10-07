import Text.Printf

solve :: [Integer] -> [Integer]
solve (a:b:rest) = [(div a b),(mod a b)]

readr :: Integer -> Integer -> IO ()
readr 0 0 = return ()
readr a b = do
    let t = solve [a,b]
    printf "%d %d / %d\n" (head t) (head (reverse t)) b
    is <- getLine
    let ns  = map read (words is) :: [Integer]
    readr (head ns) (head (reverse ns)) 

main :: IO ()
main = do
    is <- getLine
    let ns  = map read (words is) :: [Integer]
    readr ns