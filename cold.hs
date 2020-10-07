readinput = (map read) . words

solve :: [Int] -> Int
solve (s:rest) = length [y| y <- rest, y<0]

main = interact(show . solve . readinput)