import Data.List.Split (splitOn)
import Data.List

type Dot = (Integer,Integer)
data Instruction = X Integer | Y Integer deriving (Eq, Ord, Show)


main :: IO ()
main = do
  s <- readFile "input.txt"
  let (dots,instrs) = splitInput s in
      --p1:
      --print $ length $ nub (foldPaper dots (head instrs))

      --p2:
      let foldedDots = nub $ foldl foldPaper dots instrs in
          mapM_ print [[if (x,y) `elem` foldedDots then '#' else ' '|x <- [0..38]]|y <- [0..5]]

splitInput :: [Char] -> ([Dot], [Instruction])
splitInput input = (dots,instrs)
    where
        (x:y:_) = splitOn [""] (lines input)
        dots :: [Dot]
        dots = map (\n -> read ("(" ++ n ++ ")") :: (Integer,Integer)) x
        instrs :: [Instruction]
        instrs = map ((\(a:b:_) -> if a == "x" then X (read b :: Integer) else Y (read b :: Integer)) . splitOn "=" . drop 11) y

foldPaper :: [Dot] -> Instruction -> [Dot]
foldPaper dots (X a) = map (\(x,y) -> if x > a then (2*a-x,y) else (x,y)) dots
foldPaper dots (Y a) = map (\(x,y) -> if y > a then (x,2*a-y) else (x,y)) dots