
data Boomer = Opinion [Char]

ok :: Boomer -> IO ()
ok bullshitOpinion = putStrLn "Ok boomer"