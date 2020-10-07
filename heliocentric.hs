readinput = (map read) . lines

splitter (x:y:xs) = k


marsdiff n = (687-(mod n 687)) 
earthdiff n = (365-(mod n 365))

helio 0 0 g = g
helio n m g
    | (earthdiff n) == (marsdiff m) = (earthdiff n)
    | (earthdiff n) > (marsdiff m)  = helio 0 (mod (m+(earthdiff n)) 687) (g+earthdiff n)
    | otherwise = helio (mod (n+(marsdiff m)) 365) 0 (g+marsdiff m)


main = interact (splitter . readinput)