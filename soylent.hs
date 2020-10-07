 soylent :: Int -> IO ()
 soylent 0 = return ()
 soylent t = do
    n <- readLn
    print $ ceiling (n/400)
    soylent (t-1)

 main :: IO ()
 main = do
    inp <- readLn
    soylent inp