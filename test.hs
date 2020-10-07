bruh :: a -> (a -> a) -> a
bruh a b = (b) a

test1 = bruh 5 (id)

test2 = bruh False (not)

ok :: (Num a) => a
ok = 5

bro = fromIntegral ok

yo = do
        x <- getLine
        let [a,b] = words x
        putStrLn a
        putStrLn b
