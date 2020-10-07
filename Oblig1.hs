module Oblig1 where
    -- Tines Johan Kalsås Valen

import Data.List

--Oppgave A.(a)
fjern :: String -> Int -> String
fjern s num = take num s ++ drop (num+1) s

--Oppgave A.(b)
fjernc :: String -> Char -> String
fjernc s c = [x| x <- s, x /= c ]

--Oppgave A.(c)
tegnpos :: String -> Char -> [Int]
tegnpos s c = [y| (x,y) <- (zip s [0..]), x == c]

--Oppgave B.(a)
ord :: String -> [String]
ord "" = []
ord (' ':xs) = ord xs
ord str = 
    let (x,y) = break (==' ') str 
        in x:(ord y)

-- Oppgave B.(b)
tokenize :: String -> String -> String -> [String]
tokenize "" imp rem = []
tokenize (s:str) imp rem
    | s `elem` imp  = [s]:(tokenize str imp rem)
    | s `elem` rem++" "  = tokenize str imp rem
    | otherwise     =
        let (x,y) = break (`elem` (imp++rem++" ")) (s:str) 
            in x:(tokenize y imp rem)

--Oppgave C.
eqli :: Eq t => [t] -> [t] -> Bool
eqli a b = 
    let x = nub a
        y = nub b 
        in (null (x\\y) && null (y\\x))

--Oppgave D.
sjekk :: String -> String
sjekk str
        | sjekk' str "" == True = "Korrekt"
        | sjekk' str "" == False = "Feil"

sjekk' :: String -> String -> Bool
sjekk' "" ""             = True
sjekk' "" ys             = False
sjekk' (')':xs) ('(':ys) = sjekk' xs ys
sjekk' (']':xs) ('[':ys) = sjekk' xs ys
sjekk' ('}':xs) ('{':ys) = sjekk' xs ys
sjekk' (x:xs) ys = case x of
                            ')' -> False
                            ']' -> False
                            '}' -> False
                            '(' -> sjekk' xs ('(':ys)
                            '[' -> sjekk' xs ('[':ys)
                            '{' -> sjekk' xs ('{':ys)
                            x   -> sjekk' xs ys
