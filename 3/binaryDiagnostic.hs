import Data.List

main = do
  s <- readFile "input.txt"
  print $ transpose $ splitInput s

splitInput :: String -> [String]
splitInput inputs = lines inputs

mostCommonBit :: [Char] -> Char
mostCommonBit input = group
