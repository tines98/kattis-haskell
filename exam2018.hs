import Data.List


dup :: [t] -> [t]
dup [] = []
dup (x:xs) = [x,x] ++ dup xs


one :: (Eq t) => [t] -> [t] -> Bool
one xs ys = if length xs == length ys 
                then one' xs ys 0 
                else False


one' :: (Eq t) => [t] -> [t] -> Int -> Bool
one' xs ys 2    = False
one' [] [] 0    = False
one' [] [] 1    = True
one' (x:xs) (y:ys) n
    | x/=y = one' xs ys (n+1)
    | otherwise = one' xs ys n


add :: [[t]] -> [t] -> [[t]]
add [] a            = []
add (xs:xss) (a:as) = (xs++[a]):(add xss (as++[a]))


hasDuplicate :: (Eq t) => [t] -> Bool
hasDuplicate xs = hasDuplicate' xs []


hasDuplicate' :: (Eq t) => [t] -> [t] -> Bool
hasDuplicate' [] ys = False
hasDuplicate' (x:xs) ys
    | elem x ys == True = True
    | otherwise = hasDuplicate' xs (x:ys)


ok :: [Int] -> Bool
ok xs
    | hasDuplicate xs == True = False
    | length xs <= 3 = False
    | otherwise = checkOk xs

checkOk :: [Int] -> Bool
checkOk (x:[]) = True
checkOk (x:xs:xss)
    | x==xs+1 || x==xs-1 = False
    | otherwise = checkOk (xs:xss)

sol :: Int -> [[Int]]
sol n = sol' (perm n)

sol' :: [[Int]] -> [[Int]]
sol' [] = [[]]
sol' (xs:xss) = if checkOk xs 
                    then xs:sol' xss
                    else sol' xss


perm :: Int -> [[Int]]
perm n = perm' [1..n]

perm' :: [Int] -> [[Int]]
perm' [] = [[]]
perm' as = do
    a <- as
    let l = delete a as
    ls <- perm' l
    return $ a : ls

data Tre a = Blad a | Nod (Tre a) a (Tre a)

fmap :: (a -> b) -> Tre a -> Tre b 
fmap f (Blad x) = Blad (f x)
fmap f (Node ve x ho) = Node (fmap f ve) (f x) (fmap f ho)
