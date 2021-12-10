import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Function (on)


main :: IO ()
main = do
  s <- readFile "input.txt"
  let ls = splitInput s in
    print $ sortBy (compare `on` snd) $ [(x,move2 ls x)|x<-[(minimum ls)..(maximum ls)]]

splitInput :: String -> [Integer]
splitInput input = map toInt $ splitOn "," input

toInt :: String -> Integer
toInt = read

move :: [Integer] -> Integer -> Integer
move ls n = foldr (\x acc -> acc + abs (x - n)) 0 ls

move2 :: [Integer] -> Integer -> Integer
move2 ls n = foldr (\x acc -> let dist = abs (x - n) in acc + (dist * (dist + 1) `div` 2)) 0 ls