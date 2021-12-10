import qualified Data.Map as M
import Data.List.Split (splitOn)

type FishMap = M.Map Integer Integer 
baseList = [(8,0),(7,0),(6,0),(5,0),(4,0),(3,0),(2,0),(1,0),(0,0)]

main :: IO ()
main = do
  s <- readFile "input.txt"
  print $ numFish $ sim (splitInput s) 256

splitInput :: String -> FishMap
splitInput input = toFishMap (map (\x -> (toInt x,1)) (splitOn "," input))

toInt :: String -> Integer
toInt = read

toFishMap :: [(Integer,Integer)] -> FishMap
toFishMap l = M.fromListWith (+) (baseList ++ l)

fromFishMap :: FishMap -> [(Integer,Integer)]
fromFishMap = M.toList

numFish :: FishMap -> Integer
numFish fish = foldr (\x acc -> acc + snd x) 0 (fromFishMap fish)

sim :: FishMap -> Integer -> FishMap
sim fish 0 = fish
sim fish n = sim todayFish (n - 1)
  where 
    fishList = fromFishMap fish
    todayFish = simDay fish

    simDay :: FishMap -> FishMap
    simDay fish = toFishMap (map (\(x,y) -> (x-1,y)) (tail fishList) ++ [(6,snd $ head fishList),(8,snd $ head fishList)] ) 
