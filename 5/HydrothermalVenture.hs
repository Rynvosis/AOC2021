import Data.List.Split (splitOn)
import Data.List (sort,group)
import Data.Maybe (mapMaybe)

type Point = (Integer,Integer)
main :: IO ()
main = do
  s <- readFile "input.txt"
  print $ overLaplines $ splitInput s


splitInput :: String -> [[Point]]
splitInput input = mapMaybe (toOrthLine . (map toPoint . splitOn " -> ")) (lines input)
    where
        toPoint :: String -> Point
        toPoint s = let a = take 2 $ splitOn "," s in (toInt (head a), toInt (last a))

toOrthLine :: [Point] -> Maybe [Point]
toOrthLine ((x1,y1):(x2,y2):_) | x1 == x2 = Just (zip (repeat x1) ys)
                               | y1 == y2 = Just (zip xs (repeat y1))
                               | otherwise = Just (zip xs ys)
                                  where 
                                   xs = if x1 < x2 then [x1..x2] else reverse [x2..x1]
                                   ys = if y1 < y2 then [y1..y2] else reverse [y2..y1]
                                   

toOrthLine _ = error "something went wrong x_X"

overLaplines :: [[Point]] -> Int
overLaplines points = length $ filter (\x -> length x > 1) $ group $ sort $ concat points


toInt :: String -> Integer
toInt = read