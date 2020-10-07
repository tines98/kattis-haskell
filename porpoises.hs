
fibb x = (((((div ((sqrt 5)+1) 2)^x))  (((- (div ((sqrt 5)-1) 2))^x)) )/(sqrt 5))

fibb2 x = mod (fibb x) (10^9)