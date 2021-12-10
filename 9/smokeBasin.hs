import Data.List
import Data.Function (on)

main :: IO ()
main = do
  s <- readFile "input.txt"
  let input = splitInput s in
    --p1:
    --print $ getLowPoints input
    --print $ map (getPoint input) (getLowPoints input)
    --print $ riskLevel $ map (getPoint input) (getLowPoints input)

    --p2:
    --print $ getBasins input
    print $ get3Largest $ getBasins input

splitInput :: String -> [[Integer]]
splitInput s = map (map (\x -> toInt [x])) (lines s)
  where
    toInt :: String -> Integer
    toInt = read

getLowPoints :: [[Integer]] -> [(Int,Int)]
getLowPoints hMap = [(v,h)| v <- [0..99], h <- [0..99], isLowPoint v h]
  where
    isLowPoint :: Int -> Int -> Bool
    isLowPoint v h = all (>p) adjacentPoints
      where
        p = getPoint hMap (v,h)
        adjacentPoints = map (getPoint hMap) (getAdjacentPoints (v,h))

getPoint :: [[Integer]] -> (Int,Int) -> Integer
getPoint hMap (v,h) = if v == -1 || v == 100 || h == -1 || h == 100 then 9 else hMap !! v !! h

riskLevel :: [Integer] -> Integer
riskLevel = foldr (\x acc -> acc + (x+1)) 0

getAdjacentPoints :: (Int,Int) -> [(Int,Int)]
getAdjacentPoints (v,h) = [(v-1,h),(v+1,h),(v,h-1),(v,h+1)]

getBasins :: [[Integer]] -> [[(Int, Int)]]
getBasins hMap = map (\x -> checkPoints [] [x]) lowPoints
  where
    lowPoints = getLowPoints hMap

    checkPoints :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
    checkPoints inBasin checking | null validPoints = inBasin
                                 | otherwise = checkPoints newInBasin toCheck
      where
        validPoints = foldr (\x acc -> if checkPoint x then x : acc else acc) [] checking
        newInBasin = inBasin `union` validPoints
        toCheck = concatMap getAdjacentPoints validPoints

        checkPoint :: (Int,Int) -> Bool
        checkPoint point | getPoint hMap point == 9 = False
                         | point `elem` inBasin = False
                         | otherwise = True

get3Largest :: [[a]] -> Int
get3Largest ls = product $ map length $ take 3 $ sortBy (flip compare `on` length) ls
