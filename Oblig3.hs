-- Tines Valen

module Oblig3 where

import Data.Char
import Data.Bits
--TODO
import Data.Typeable

type Board = [Int]


next :: Int -> Int
next 1 = 2
next 2 = 1


initial :: Int -> Board
initial x = [1..x]


chompInitial :: Int -> Board
chompInitial x = replicate x x


finished :: Board -> Bool
finished = all (== 0)

chompFinished :: Board -> Bool
chompFinished board = (board !! ((length board)-1)) == 0


valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num


move :: Board -> Int -> Int -> Char -> Board
move board row num 'n' = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n
move board row col 'c' = [update r c | (r,c) <- zip [1..] board]
    where update r c = if r <= row then (min c (col-1)) else c


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard :: Board -> Int -> IO ()
putBoard board n = do 
    putBoard' board n
    putStr "   "
    putStr (unwords (map show [1..(length board)]))
    newline
    

putBoard' :: Board -> Int -> IO ()
putBoard' [] n       = return ()
putBoard' (x:xs) n   = do
                        putRow n x
                        putBoard' xs (n+1)


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do  putStrLn "ERROR: Invalid digit"
                            getDigit prompt


newline :: IO ()
newline = putChar '\n'


help :: Char -> IO ()
help 'n' = putStrLn "players take turns picking 'stones' from a heap, you must remove at least 1 stone, the player that takes the last stone, wins"
help  c  = putStrLn "player take turns 'biting' a chocolate bar, but the lower left bar is poisoned, the player that eats it, loses"


allDigits :: String -> Bool
allDigits s = takeWhile isDigit s == s


getWin :: Char -> Int -> IO ()
getWin 'n' player = win (next player)
getWin 'c' player = win player

win :: Int -> IO ()
win 1 = do
    putStr "Player 1 wins!!"
    newline
win 2 = do 
    putStr "CPU wins!!"
    newline


prompt :: Char -> IO ()
prompt 'n' = putStrLn "Nim: r a / ? / q >"
prompt 'c' = putStrLn "Chomp: r k / ? / q >"


generiskSpill :: Board -> Int -> Char -> IO ()
generiskSpill board player game=
    do  newline
        putBoard board 1
        if finished board 
            then getWin game player
        else do
            if player == 1 then do
                newline
                putStr "Player "
                putStrLn (show player)
                prompt game
                l <- getLine
                let r = head l
                let a = if allDigits (last (words l)) 
                    then (read (last (words l)) :: Int)
                    else 0
                if a > (length board)
                    then do
                        newline
                        putStr "ERROR: please input value under "
                        putStr (show (length board))
                        generiskSpill board player game
                    else if isDigit r then do
                        if (digitToInt r) <= (length board) && valid board (digitToInt r) a && a > 0 
                            then generiskSpill (move board (digitToInt r) a game) (next player) game
                        else do
                            putStrLn "ERROR: Invalid move"
                            generiskSpill board player game
                    else do
                        command r game
                        generiskSpill board player game
            else botPlay board game


botPlay :: Board -> Char -> IO ()
botPlay board game= do
    putStrLn "CPU's turn"
    let ns = nimSum board
    if ns == 0 || game=='c'
        then 
            generiskSpill (move board (checkBoard board 1 1) 1 game) 1 game
        else do 
            let (r,c) = bruteForce board 1 1 game
            generiskSpill (move board r c game) 1 game
    

checkBoard :: Board -> Int -> Int -> Int
checkBoard board rem row =  if (length board) >= row
                                then if  (board !! (row-1)) >= rem 
                                    then row
                                    else checkBoard board rem (row+1)
                            else -1


bruteForce :: Board -> Int -> Int -> Char-> (Int,Int)
bruteForce board r c game
    | r > length board = bruteForce board 1 (c+1) game
    | c > (board !! (r-1))    = bruteForce board (r+1) c game
    | nimSum (move board r c game) == 0 = (r,c)
    | otherwise = bruteForce board (r+1) c game


command :: Char -> Char -> IO ()
command '?' game = help game
command 'q' game = spill
command  k  game  = putStrLn "ERROR INVALID COMMAND"


nim :: Int -> IO ()
nim x = generiskSpill (initial x) 1 'n'


chomp :: Int -> IO ()
chomp x = generiskSpill (chompInitial x) 1 'c'


nimSum :: Board -> Int
nimSum (x:[]) = x
nimSum (x:xs) = xor x (nimSum xs)


spill :: IO ()
spill = do
    putStrLn "n(im) x / c(homp) x / q(uit) >"
    l <- getLine
    let g = head l
    if length l == 1 && g == 'q'
        then spill
        else do
            if g == 'n' || g == 'c'
                then do
                    let x = (last (words l))
                    if allDigits x
                        then spillCommand g (read x :: Int)
                    else spill
            else spill
                    

spillCommand :: Char -> Int -> IO()
spillCommand 'n' x = nim x
spillCommand 'q' x = return()
spillCommand 'c' x = chomp x
spillCommand  c  x = error "ERROR: Invalid Command"

