import Data.List (sort)
main :: IO ()
main = do
  s <- readFile "input.txt"
  let input = lines s in
    --p1:
    --print $ foldr (\x acc -> acc + corruptedLines x) 0 input

    --p2:
    print $ middle $ sort [autoCompleteLines x | x <- input, corruptedLines x == 0 ]

corruptedLines :: String -> Integer
corruptedLines input = stackCheck [] input 
    where
        stackCheck :: [Char] -> String -> Integer
        stackCheck [] [] = 0 -- valid line
        stackCheck _ [] = 0  -- incomplete line
        stackCheck [] (y:ys) = stackCheck [y] ys
        stackCheck (x:xs) (y:ys) | isOpen y = stackCheck (y : x : xs) ys
                                 | isPair x y = stackCheck xs ys
                                 | otherwise = errorScore y
            where
                isOpen x = x `elem` ['(','[','{','<']
                isPair x y = (x,y) `elem` [('(',')'),('[',']'),('{','}'),('<','>')]
                errorScore x | x == ')' = 3
                             | x == ']' = 57
                             | x == '}' = 1197
                             | x == '>' = 25137
                             | otherwise = 0

autoCompleteLines :: String -> Integer
autoCompleteLines input = stackCheck [] input 
    where
        stackCheck :: [Char] -> String -> Integer
        stackCheck [] [] = 0
        stackCheck stack [] = autoCompleteScore stack 0
            where 
                autoCompleteScore :: [Char] -> Integer -> Integer
                autoCompleteScore [] score = score
                autoCompleteScore (x:xs) score = autoCompleteScore xs (5 * score + charScore x)
                    where 
                        charScore x | x == '(' = 1
                                    | x == '[' = 2
                                    | x == '{' = 3 
                                    | x == '<' = 4
                                    | otherwise = 0
        stackCheck [] (y:ys) = stackCheck [y] ys
        stackCheck (x:xs) (y:ys) | isOpen y = stackCheck (y : x : xs) ys
                                 | isPair x y = stackCheck xs ys
                                 | otherwise = 0
            where
                isOpen x = x `elem` ['(','[','{','<']
                isPair x y = (x,y) `elem` [('(',')'),('[',']'),('{','}'),('<','>')]

middle = m =<< drop 1
   where m []  = take 1
         m [_] = take 2
         m (_:_:ys) = m ys . drop 1 