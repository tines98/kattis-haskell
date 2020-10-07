facto :: Int -> Int
facto(n)
	| n == 0 = 1
	| otherwise = n * facto(n-1)

fibb :: Int -> Int
fibb(n)
	| n <= 1 = 1
	| otherwise = fibb(n-1) + fibb(n-2)

factor n = if n <= 0 then 1 else n * factor (n-1)

fibbo n = if n <= 1 then 1 else (fibbo(n-1) + fibbo(n-2))
