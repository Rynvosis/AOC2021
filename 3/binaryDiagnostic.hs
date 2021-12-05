import Data.List
import Data.Function (on)

main :: IO ()
main = do
  s <- readFile "input.txt"
  print (lifeSupportRating s)

powerConsumption :: String -> Integer
powerConsumption s = binaryToDecimal (read (map mostCommonBit $ transpose $ lines s) :: Integer) * binaryToDecimal (read (map leastCommonBit $ transpose $ lines s) :: Integer)

mostCommonBit :: String -> Char
mostCommonBit input = head $ maximumBy(compare `on` length) $ group $ sort input

leastCommonBit :: String -> Char
leastCommonBit input = head $ minimumBy(compare `on` length) $ group $ sort input


lifeSupportRating :: String -> Integer
lifeSupportRating s = (binaryToDecimal $ read (oxygenRating (lines s) 0) :: Integer) * (binaryToDecimal $ read (co2Rating (lines s) 0) :: Integer)

oxygenRating :: [String] -> Int -> String
oxygenRating inputs i | length(head inputs) > i = oxygenRating (filter (\x -> x!!i == mostCommon) inputs) (i+1)
                      | otherwise = head inputs
                       where mostCommon = mostCommonBit $ transpose inputs !! i

co2Rating :: [String] -> Int -> String
co2Rating inputs i | length(head inputs) > i = co2Rating (filter (\x -> x!!i == leastCommon) inputs) (i+1)
                   | otherwise = head inputs
                    where leastCommon = leastCommonBit $ transpose inputs !! i

binaryToDecimal :: Integer -> Integer
binaryToDecimal 0 = 0
binaryToDecimal x = 2 * binaryToDecimal (x `div` 10) + x `mod` 10
