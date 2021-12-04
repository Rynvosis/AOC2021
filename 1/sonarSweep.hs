main = do
  s <- readFile "input.txt"
  print $ sweep (splitToInts s) 0

splitToInts :: String -> [Integer]
splitToInts inputs = map (\x -> read x :: Integer) (lines inputs)\

threeSlidingWindow :: [Integer] -> [Integer]
threeSlidingWindow (x:y:[]) = []
threeSlidingWindow inputs = sum (take 3 inputs) : threeSlidingWindow (tail inputs)

sweep :: (Num int) => [Integer] -> int -> int
sweep [] acc = acc
sweep [x] acc = acc
sweep (x:xs) acc | x < head xs = sweep xs (acc + 1)
                 | otherwise = sweep xs acc