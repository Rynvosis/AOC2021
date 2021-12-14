import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function (on)
import Distribution.SPDX.LicenseId (LicenseId(IJG))

type Rules = Map String [String]
type Polymer = Map String Int


main :: IO ()
main = do
  s <- readFile "input.txt"
  let input@(polymer,rules) = splitInput s in
      --p1:
      --mapM_ print $ M.toList polymer
      --mapM_ print $ take 2 (iterate (step rules) polymer)
      print $ charFreq (iterate (step rules) polymer !! 40)

--splitInput :: String -> (Polymer,[Rule])
splitInput :: String -> (Polymer, Rules)
splitInput input = (polymer,rules)
    where
        ((x:_):y:_) = splitOn [""] (lines input)

        polymer = M.fromListWith (+) ((map (\l -> (l,1)) (divvy 2 1 x)) ++ [(last x : " ",1)])

        rules = M.fromList ((map ((\((a:b:_):(c:_):_) -> ([a,b],[[a,c],[c,b]])) . splitOn " -> ") y) ++ [(last x : " ",[last x : " "])])

step :: Rules -> Polymer -> Polymer
step rules polymer = M.fromListWith (+) $ M.foldrWithKey (\k x acc -> map (\n -> (n,x)) (rules M.! k) ++ acc ) [] polymer

charFreq :: Polymer -> Int
charFreq polymer = 
    let ls = sortBy (compare `on` snd) $ M.toList $ M.fromListWith (+) $ M.foldrWithKey (\k x acc -> (head k,x) : acc ) [] polymer in
        snd (last ls) - snd (head ls)