readinput = (map read) . words

solve :: [Double] -> Double
solve (cost:x:h:w:[]) = (cost*h*w)
solve (cost:x:h:w:rest) = (cost*h*w) + solve (cost:x:rest)

main = interact(show . solve . readinput)