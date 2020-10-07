-- Tines Valen

module Oblig2 where

import Data.Char

data Ast = Tall Int | Sum Ast Ast | Mult Ast Ast | Min Ast | Var String deriving (Eq, Show)

--Oppgave 1

onlyDigits x = takeWhile isDigit x == x 

parse :: String -> Ast
parse str = if null rest then expr else error ("invalid syntax")
    where
        (expr, rest) = (parseExpr (tokenize str "*+-" ""))


parseExpr :: [String] -> (Ast,[String])
parseExpr ("+":ss)    = let (x,xs) = parseExpr ss
                            (y,ys) = parseExpr xs 
                            in ((Sum x y),ys)
parseExpr ("-":ss)    = let (x,xs) = parseExpr ss
                            in (Min x, xs)
parseExpr ("*":ss)    = let (x,xs) = parseExpr ss
                            (y,ys) = parseExpr xs 
                            in ((Mult x y),ys)
parseExpr (s:ss)
    | onlyDigits s = (Tall (read s :: Int),ss)
    | otherwise = (Var s,ss)


tokenize :: String -> String -> String -> [String]
tokenize "" imp rem = []
tokenize (s:str) imp rem
    | s `elem` imp  = [s]:(tokenize str imp rem)
    | s `elem` rem++" "  = tokenize str imp rem
    | otherwise     =
        let (x,y) = break (`elem` (imp++rem++" ")) (s:str) 
            in x:(tokenize y imp rem)


--Oppgace 2

vis :: Ast -> IO ()
vis ast = putStr (viss ast)

viss :: Ast -> String
viss ast = viss' ast 3

viss' :: Ast -> Int -> String
viss' (Sum a b) x   = "Sum\n" ++ (replicate x ' ') ++ (viss' a (x+3)) ++ (replicate x ' ') ++ (viss' b (x+3))
viss' (Mult a b) x  = "Mult\n" ++ (replicate x ' ') ++ (viss' a (x+3)) ++ (replicate x ' ') ++ (viss' b (x+3))
viss' (Min a) x     = "Min\n" ++ (replicate x ' ') ++ (viss' a (x+3))
viss' (Tall a) x    = "Tall " ++ show a ++ "\n"
viss' (Var a) x     = "Var " ++ a ++ "\n"


--Oppgave 3

trav :: Ast -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Int -> a) -> Int -> a
trav (Sum x y) sum mult min tall z  = sum (trav x sum mult min tall z) (trav y sum mult min tall z)
trav (Mult x y) sum mult min tall z = mult (trav x sum mult min tall z) (trav y sum mult min tall z)
trav (Min x) sum mult min tall z    = min (trav x sum mult min tall z)
trav (Tall x) sum mult min tall z   = tall x
trav (Var x) sum mult min tall z    = tall z

evi :: Ast -> Int
evi ast    = trav ast (+) (*) (negate) (id) 0

evb :: Ast -> Bool
evb ast    = trav ast (||) (&&) (not) (odd) 0


--Oppgave 4

evix :: Ast -> Int -> Int
evix ast x    = trav ast (+) (*) (negate) (id) x

evbx :: Ast -> Int -> Bool
evbx ast x    = trav ast (||) (&&) (not) (odd) x