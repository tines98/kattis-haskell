myLast :: [t] -> t
myLast xs = head (reverse xs)

myButLast :: [t] -> t
myButLast xs = head (tail (reverse xs))

elementAt :: [t] -> Int -> t
elementAt xs i = xs !! i

myLength :: [t] -> Int
myLength [] = 0
myLength (x:xs) = 1+myLength xs 

myReverse :: [t] -> [t]
myReverse xs = foldr (\x y -> y++[x]) [] xs


isPalindrome :: Eq t => [t] -> Bool
isPalindrome xs = myReverse xs == xs

data NestedList a = Elem a | List [NestedList a]

-- createNL = [Elem 1:List [Elem 2,Elem 4]]

flatten :: NestedList t -> [t]
flatten (Elem i   )     = [i]
flatten (List (x:xs))   = flatten x ++ flatten (List xs)
flatten (List [])       = []

-- remdups :: (Eq t) => [t] -> [t]
-- remdups [] = []
-- remdups xs = foldr remdup [] xs

-- remdup :: (Eq t) => t -> [t] -> [t]
-- remdup x [] = [x]
-- remdup x ys = if x == head ys then ys else x:ys

-- pack :: Eq t => [t] -> [[t]]
-- pack xs = foldr elemPut [[]] xs

-- elemPut x (xs:xss) = if x == xs then (x:xs):xss else 

-- -- data Eleven = Multiple Int t | Single t

-- -- encodeModified :: [t] -> []

-- insertAt :: t -> [t] -> Int -> [t]
-- insertAt x xs i = take (i-1) xs ++ [x] ++ drop (i-1) xs

-- range :: Int -> Int -> [Int]
-- range n m = [n..m]


-- sumOfSquares :: Int -> Int
-- sumOfSquares x = sumOfSquares' [1..x]

-- sumOfSquares' :: [Int] -> Int
-- sumOfSquares' (x:xs) = x*x + sumOfSquares' xs
-- sumOfSquares' [] = 0

-- sumSq :: Int -> Int
-- sumSq n = foldr (\x xs -> x*x+xs) 0 [1..n]

findIndices :: Char -> [Char] -> [Int]
findIndices chr str = findIndices' chr str 0

findIndices' :: Char -> [Char] -> Int -> [Int]
findIndices' chr (x:xs) n
    | x==chr = n:(findIndices' chr xs (n+1))
    | otherwise = findIndices' chr xs (n+1)
findIndices' chr [] n = []

map2 :: (a -> b) -> [a] -> [b]
map2 func (x:xs) = (func x):(map2 func xs)
map2 func [] = []


