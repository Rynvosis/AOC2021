import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isLower, isUpper)


main :: IO ()
main = do
  s <- readFile "input.txt"
  let input = splitInput s in
      --p1:
      print $ allValidPaths input True
      --print $ length $ allValidPaths input 2

splitInput :: String -> Map String [String]
splitInput input = M.fromListWith (++) (concatMap (getTups . splitOn "-") (lines input))
    where
        getTups :: [String] -> [(String,[String])]
        getTups (x:y:_)| x == "start" || y == "end" =  [(x,[y])]
                       | x == "end" || y == "start" = [(y,[x])]
                       | otherwise = [(x,[y]),(y,[x])]
        getTups _ = error "something went wrong x_x"

--allValidPaths :: Map String [String] -> Bool -> Int
allValidPaths nodeMap canVisitExtra = validPath ["start"]
    where
        smalls :: [String]
        smalls = filter (\x -> isLower (head x) && x /= "start") (M.keys nodeMap)

        smallTo :: Map String (Map String Int)
        smallTo = M.fromList [(x,M.fromList [(y,smallPath y [x])| y <- "end":smalls])| x <- "start":smalls]
            where
                smallPath :: String -> [String] -> Int
                smallPath to path@(l:ls) | l == to && length path > 1 = 1
                                         | isLower (head l) && length path > 1 = 0
                                         | otherwise = sum $ map (smallPath to . (:path)) (nodeMap M.! l)
                smallPath _ _ = error "small path given invalid list"

        --validPath :: [String] -> Int
        validPath path@(x:xs) = smallTo M.! x M.! "end" + sum (map (\y -> (smallTo M.! x M.! y) * validPath (y:path)) nextNodes)
            where 
                nextNodes :: [String]
                nextNodes | canVisitExtra && length path == length (nub path) = delete "end" $ M.keys (smallTo M.! x)
                          | otherwise = smalls \\ path
        validPath _ = error "shit"