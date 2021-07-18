cdoubleMe x = x + x

-- Produces a list from 1 to x
getListInRange :: Int -> [Int]
getListInRange x = [1..x]

-- Factorial - Multiply elements in list
factorial :: Int -> Int
factorial n = product [1..n]

-- Factorial - Recursion
factorialRec :: Int -> Int
factorialRec 0 = 1 -- Base Case
factorialRec n = n * factorialRec(n - 1)

-- Functions
circumfrence :: Float -> Float
circumfrence r = 2 * pi * r

-- Polymorphic Functions 
--  - Functions that use type variables (basically generics)

-- Pattern Matching
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

-- List Comprehension
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Multiple parameters
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + x