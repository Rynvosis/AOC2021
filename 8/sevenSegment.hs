import Data.List.Split (splitOn)
import Data.List
import qualified Data.Map as M

main :: IO ()
main = do
  s <- readFile "input.txt"
  let input = splitInput s in
    print $ decodeInput input

splitInput :: String -> [[[String]]]
splitInput input = map (map (splitOn " ") . splitOn " | ") (lines input)

digits1478AppearRate :: [[[String]]] -> Int
digits1478AppearRate =
  foldr (\x acc -> acc + foldr (\x acc -> acc + digitIs1478 x) 0 (last x)) 0
    where
      digitIs1478 :: String -> Int
      digitIs1478 s | length s `elem` [2,3,4,7] = 1
                    | otherwise = 0

decodeInput :: [[[String]]] -> Integer
decodeInput = foldr (\x acc-> acc + decodeNums (decodeSignalPatterns (head x)) (last x) 0) 0 

decodeSignalPatterns :: [String] -> M.Map Char Char
decodeSignalPatterns input = M.fromList [(a,'a'),(b,'b'),(c,'c'),(d,'d'),(e,'e'),(f,'f'),(g,'g')]
  where
    a = head $ (lengFreq M.! 3) \\ (lengFreq M.! 2)
    b = head $ charFreq M.! 6
    c = head $ (lengFreq M.! 2) `intersect` (charFreq M.! 8)
    d = head $ (lengFreq M.! 4) \\ [b,c,f]
    e = head $ charFreq M.! 4
    f = head $ (lengFreq M.! 2) \\ [c]
    g = head $ (lengFreq M.! 7) \\ [a,b,c,d,e,f]

    charFreq = invertMap $ M.fromListWith (+) $ map (\x -> ([x],1)) (concat input)
    lengFreq = invertMap $ M.fromList $ map (\x -> (x,toInteger $ length x)) input

    invertMap :: M.Map String Integer -> M.Map Integer String
    invertMap input = M.fromListWith (++) $ map (\(x,y) -> (y,x)) $ M.toList input

decodeNums :: M.Map Char Char -> [String] -> Integer -> Integer
decodeNums m [] acc = acc
decodeNums m (n:ns) acc = decodeNums m ns (10 * acc + sevenSegToInt (sort (decodeNum m n)))
  where
    decodeNum :: M.Map Char Char -> String -> String
    decodeNum m [] = []
    decodeNum m (x:xs) = m M.!x : decodeNum m xs

sevenSegToInt :: String -> Integer
sevenSegToInt segs | segs == "abcefg" = 0
                   | segs == "cf" = 1
                   | segs == "acdeg" = 2
                   | segs == "acdfg" = 3
                   | segs == "bcdf" = 4
                   | segs == "abdfg" = 5
                   | segs == "abdefg" = 6
                   | segs == "acf" = 7
                   | segs == "abcdefg" = 8
                   | segs == "abcdfg" = 9
                   | otherwise = error "Something went wrong X_x"