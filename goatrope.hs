readInput = (map read) . words

solve :: [Int] -> Int
solve (x1:y1:x2:y2:x3:y3:rest) = do
    let dx = max (abs (x1-(x2+(x3-x2)/2))-(x3-x2)) 0
    let dy = max (abs (y1-(y2+(y3-y2)/2))-(y3-y2)) 0
    (dx * dx) + (dy * dy)

main = interact(writeOutput . solve . readInput)

writeOutput = unlines . (map show)