readinput = (map read) . words

solve :: [Int] -> Int
solve (a:i:rest) = ((a * (i-1)) + 1)

main = interact(show . solve . readinput)