{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List.Split (splitOn)
import Data.List.Split.Internals (chunksOf)
import Data.List (transpose, find)

type Cell = (Integer,Bool)
type Line = [Cell]
type Board = [Line]

main :: IO ()
main = do
  s <- readFile "input.txt"
  let (draw,boards) = splitInputs s in
    print $ lastWinningBoard draw boards []


--Input Handling
splitInputs :: String -> ([Integer],[Board])
splitInputs s = (head draw : draw,boards)
                  where
                    inputs = lines s
                    draw = map toInt (splitOn "," (head inputs))
                    boards = parseBoards (tail inputs)

toInt :: String -> Integer
toInt = read

parseBoards :: [String] -> [Board]
parseBoards inputs = map (map (map (\x -> (toInt x,False)) . filter (/= "") . splitOn " ") . tail) (chunksOf 6 inputs)


-- Game Handling
firstWinningBoard :: [Integer] -> [Board] -> (Board,Integer)
firstWinningBoard [] _ = error "No Winning Boards"
firstWinningBoard (x:xs) boards =
  let playedBoards = playNum x boards
  in case winningBoard playedBoards of
      Nothing -> firstWinningBoard xs playedBoards
      Just n -> (n,boardScore x n)

lastWinningBoard :: [Integer] -> [Board] -> [Board] -> (Board,Integer)
lastWinningBoard [] _ _ = error "No Winning Boards"
lastWinningBoard (x:_) [] wonBoards = 
  let lastBoard = last wonBoards in
    (lastBoard,boardScore x lastBoard)
lastWinningBoard (_:x:xs) boards wonBoards =
  let playedBoards = playNum x boards in 
    lastWinningBoard (x:xs) (filter hasNotLine playedBoards) (wonBoards ++ filter hasLine playedBoards)


boardScore :: Integer -> Board -> Integer
boardScore x board = x * foldr (\(n,bool) acc -> if bool then acc else acc + n) 0 (concat board)


-- Checking the board for a number and marking it as true
playNum :: Integer -> [Board] -> [Board]
playNum x = map (map (map (playCell x)))

playCell :: Integer -> Cell -> Cell
playCell x (n,b) | b || x == n = (n,True)
                 | otherwise = (n,False)


-- Board Checking
winningBoard :: [Board] -> Maybe Board
winningBoard = find hasLine

winningBoards :: [Board] -> [Board]
winningBoards = filter hasLine

hasLine :: Board -> Bool
hasLine board = horizontalLines board || verticleLines board

hasNotLine :: Board -> Bool
hasNotLine board = not (hasLine board)

horizontalLines :: Board -> Bool
horizontalLines = any checkLine

verticleLines :: Board -> Bool
verticleLines b = horizontalLines $ transpose b

checkLine :: Line -> Bool
checkLine = foldl(\acc x -> snd x && acc) True