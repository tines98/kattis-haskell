f :: Int -> Int
f 0 = 0
f x = x^2 + f (x-1)

g x = if x == 0 then 1 else x^2 + f (x-1) 


toList :: Int -> [Int]
toList 0 = []
toList x = toList (div x 10) ++ [mod x 10]



abtwo :: Int -> Int -> Char -> Bool
abtwo a b s 
    | s == 'a'  = ((a+1) == b)
    | s == 'b'  = (a == (b+1))
    | otherwise = (a == b)

ab :: Int -> Int -> String -> Bool
ab a b (s:ss)
    | length ss == 0  = abtwo a b s
    | s == 'a'        = ab (a+1) b ss
    | s == 'b' && a == b = False
    | s == 'b'        = ab a (b+1) ss
    | otherwise       = ab a b ss