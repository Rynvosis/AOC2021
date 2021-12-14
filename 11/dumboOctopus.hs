import Data.List

main :: IO ()
main = do
  s <- readFile "input.txt"
  let input = splitInput s in
      --p1:
      --mapM_ print $ iterate stepCount (input,0) !! 100

      --p2:
      print $ synchronizedFlash input 0

splitInput :: String -> [[Integer]]
splitInput s = map (map (\x -> toInt [x])) (lines s)
  where
    toInt :: String -> Integer
    toInt = read

step :: [[Integer]] -> [[Integer]]
step grid = flash (incrPoints grid [(v,h)| v <- [0..9], h <- [0..9]]) []
  where
    flash :: [[Integer]] -> [(Int,Int)] -> [[Integer]]
    flash octopuses flashed = let x = getToFlash in 
        case x of 
            [] -> setPoints octopuses flashed 0 
            _ -> flash (foldr (\x acc -> incrPoints acc (getAdjacentPoints x)) octopuses getToFlash) (flashed `union` getToFlash)
        where 
            getToFlash = [(v,h)| v <- [0..9], h <- [0..9], octopuses !! v !! h > 9 ] \\ flashed

stepCount :: ([[Integer]],Int) -> ([[Integer]],Int)
stepCount (grid,count) = flash (incrPoints grid [(v,h)| v <- [0..9], h <- [0..9]]) []
  where
    flash :: [[Integer]] -> [(Int,Int)] -> ([[Integer]],Int)
    flash octopuses flashed = let x = getToFlash in 
        case x of 
            [] -> (setPoints octopuses flashed 0,count + length flashed)
            _ -> flash (foldr (\x acc -> incrPoints acc (getAdjacentPoints x)) octopuses getToFlash) (flashed `union` getToFlash)
        where 
            getToFlash = [(v,h)| v <- [0..9], h <- [0..9], octopuses !! v !! h > 9 ] \\ flashed
            
synchronizedFlash :: [[Integer]] -> Int -> Int
synchronizedFlash grid count | isSynchronized = count
                             | otherwise = synchronizedFlash (step grid) (count + 1)
    where
        isSynchronized = all (all (== 0)) grid




incrPoints :: [[Integer]] -> [(Int,Int)] -> [[Integer]]
incrPoints grid toChange = [[setValue (v,h) | h <- [0..9] ] | v <- [0..9]]
    where
        setValue :: (Int,Int) -> Integer 
        setValue (v,h) | (v,h) `elem` toChange = grid !! v !! h + 1
                       | otherwise = grid !! v !! h

setPoints :: [[Integer]] -> [(Int,Int)] -> Integer -> [[Integer]]
setPoints grid toChange setTo = [[setValue (v,h) | h <- [0..9] ] | v <- [0..9]]
    where
        setValue :: (Int,Int) -> Integer 
        setValue (v,h) | (v,h) `elem` toChange = setTo
                       | otherwise = grid !! v !! h


getAdjacentPoints :: (Int,Int) -> [(Int,Int)]
getAdjacentPoints (v,h) = [(y,x)| y <- [v-1,v,v+1],x <- [h-1,h,h+1], (y,x) /= (v,h)]