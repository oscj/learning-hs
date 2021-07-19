import Data.List
import Data.Maybe

-- Given an array of integers nums and an integer target, 
-- return indices of the two numbers such that they add up to target
twoSum :: (Eq a, Num a) => [a] -> a -> [(Int, Int)]
twoSum nums target = map (\(x,y) -> (validIndicies x y nums)) (validPairs nums target)
  where
    validPairs n t = filter (\(x,y) -> x + y == t) (pairs n)
    validIndicies x y n = (fromJust(findIndex (==x) n), fromJust (findIndex (==y) n))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]



-- Given an integer numRows, return the first numRows of Pascal's triangle.
-- For example, given numRows = 5,
-- Return [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
pascalsTriangle :: Int -> [[Int]]
pascalsTriangle nr = map (\x -> pascalRow x) [1..nr]

pascalRow ::  Int -> [Int]
pascalRow 1 = [1]
pascalRow 2 = [1,1]
pascalRow n = [1] ++ sumPairedList (pascalRow (n-1)) ++ [1]

sumPairedList :: [Int] -> [Int]
sumPairedList [a,b] = [a + b]
sumPairedList (x : xs) = [x + head xs] ++ sumPairedList xs