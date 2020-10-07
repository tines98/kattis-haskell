solve :: Int -> Int -> Int -> IO ()
solve 0 b tries = return ()
solve tot b tries = do
    let nb = div b 2
    let ntot = tot - nb
    solve ntot nb (tries+1)

main :: IO ()
main = do
    ks <- getLine
    let k = read ks :: Int
    let a = 2^(ceiling(logBase 2 k))
    solve k a 0
