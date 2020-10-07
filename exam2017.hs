

data BT = B Int | N BT Int BT

harEl :: (t -> Bool) -> [t] -> Bool
harEl pr []     = False
harEl pr (x:xs) = if pr x then True else harEl pr xs


el :: (t -> Bool) -> [t] -> t
el pr []     = error("not in list")
el pr (x:xs) = if pr x then x else el pr xs

gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep pr y []        = []
gRep pr y (x:xs)    = 
                    if pr x 
                        then (y:gRep pr y xs)
                        else (x:gRep pr y xs)

elt :: BT -> Int -> Bool
elt (B bx) x = if x==bx then True else False
elt (N ltr nx rtr) x = 
                    if nx==x 
                        then True
                        else elt ltr x || elt rtr x


toL :: BT -> [Int]
toL (B x)           = [x]
toL (N ltr x rtr)   = toL ltr ++ [x] ++ toL rtr

-- data Graph = Edge 

-- main :: IO ()
-- main = do
--     x <- getLine

-- graphio ::

-- dup :: BT -> Bool
-- dup tr = 
--     if nub l == l then True else False
--         where l = toL tr

nub :: Eq t => [t] -> [t]
nub [] = []
nub (x:xs) = if contains xs x then nub xs else x:nub xs


contains :: Eq t => [t] -> t -> Bool
contains [] a = False
contains (x:xs) a = if x == a then True else contains xs a 

naboL :: Eq t => [(t,t)] -> [(t,[t])]
naboL xs = nub (foldl (\acc (x,y) -> (x,(findNeigh xs x [])):acc) [] xs)

findNeigh :: Eq t => [(t, a)] -> t -> [a] -> [a]
findNeigh [] a acc = acc
findNeigh ((x,y):xs) a acc = if x == a then findNeigh xs a (y:acc) else findNeigh xs a acc


kantL :: [(t,[t])] -> [(t,t)]
kantL [] = []
kantL (nl:nls) = kantL' nl ++ kantL nls

kantL' :: (t,[t]) -> [(t,t)]
kantL' (n,[])     = []
kantL' (n,(k:[])) = [(n,k)]
kantL' (n,(k:ks)) = (n,k):kantL' (n,ks)

-- naboer :: Eq t => [(t,[t])] -> t -> [t]
-- naboer [] a = error("no such node")
-- naboer ((x,xs):nl) a = if x==a then xs else naboer nl a

naboer x nL = if (harEl ((== x).fst) nL) then snd (el ((== x).fst) nL) else []

findKant :: Eq t => [(t,t)] -> t -> (t,t)
findKant ((a,b):[]) x = if a == x then (a,b) else (x,x)
findKant ((a,b):kl) x = if a == x then (a,b) else findKant kl x

findKanter :: Eq t => [(t,t)] -> t -> [(t,t)]
findKanter kl x = foldr (\(a,b) acc -> if a==x then ((a,b):acc) else acc) [] kl

cyc nL x = let re = trav nL [] x in
                    if null re 
                        then [] 
                        else [reverse (head re)]

trav nL vis x = if (elem x vis) 
                    then [x:vis]
                    else concat (map (trav nL (x:vis) x) (naboer nL x))