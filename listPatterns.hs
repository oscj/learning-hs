-- recursion to get evens
filterEvens0 :: [Int] -> [Int]
filterEvens0 []        = []
filterEvens0 (x:xs)
 | mod x 2 == 0        = x : filterEvens1 xs
 | otherwise           = filterEvens1 xs

-- list filter to get evens
filterEvens1 :: [Int] -> [Int]
filterEvens1 xs = filter (\(x) -> mod x 2 == 0) xs

-- Sum recursively
sumElems0 :: [Int] -> Int
sumElems0 [] = 0
sumElems0 (x:xs) = x + sumElems0(xs)

-- Sum via fold (reduce)
sumElems1 :: [Int] -> Int
sumElems1 xs = foldl (\acc x -> acc + x) 0 xs


