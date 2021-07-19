-- Exercise solutions 
-- List Exercises from Haskell For Imperative Programmers, Video #5
-- Link: https://www.youtube.com/watch?v=Cxkqrg8FCt8
--
-- July 18 2021

-- 1. Create a function that returns True if an element 
--    is in a given list and returns False otherwise
isElemInList :: (Eq a) => a -> [a] -> Bool
isElemInList x xs = not (filter (== x) xs == [])

-- 2. Create a function that removes all duplicates from a given list
--    and returns the result
removeDups :: (Eq a) => [a] -> [a]
removeDups [a,b]      = if a == b then [a] else [a,b]
removeDups (x:xs)     = nonDup ++ removeDups xs
    where nonDup      = if isElemInList x xs then [] else [x] 

-- 3. Create a function isAscending that returns True 
--    if a given list is in ascending order, and False otherwise
isAscending :: [Int] -> Bool
isAscending []        = True
isAscending [a]       = True
isAscending [a,b]     = (a <= b)
isAscending (x:xs)    = (x <= head xs && isAscending xs)

-- 4. Create a function hasPath that determines if a path
--    from one node to another exists within a directed graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y         = False
hasPath dirG start end = not ((filter (==end) (possibleEndings dirG start)) == [])

possibleEndings :: [(Int, Int)] -> [Int]
possibleEndings dirG startNode
    | succs == []                        = []
    | isStuckInLoop                      = infinteCycleNodes
    | otherwise                          = allEndings
    where
        succs = getSuccessors dirG startNode
        isStuckInLoop = length succs == 1 && length (getSuccessors dirG (succs !! 0)) == 1
        infinteCycleNodes = [succs !! 0, (getSuccessors dirG (succs !! 0)) !! 0]
        allEndings = succs ++ foldl (\acc x -> acc ++ x) [] (map (\(a) -> possibleEndings dirG a) succs)

getSuccessors :: [(Int, Int)] -> Int -> [Int]
getSuccessors dirG node = map (\(a,b) -> b) (filter (\(a,b) -> a == node) dirG)