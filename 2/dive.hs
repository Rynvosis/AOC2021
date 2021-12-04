main = do
  s <- readFile "input.txt"
  let a = diveAim (splitInput s) (0,0) 0
    in print (fst a * snd a)

splitInput :: String -> [(String,Integer)]
splitInput inputs = map (\[x,y] -> (x,read y :: Integer)) (map words $ lines inputs)

dive :: [(String,Integer)] -> (Integer,Integer) -> (Integer,Integer)
dive [] acc = acc
dive ((x,y):xs) (a,b) | x == "forward" = dive xs (a+y,b)
                      | x == "up" = dive xs (a,b-y)
                      | x == "down" = dive xs (a,b+y)

diveAim :: [(String,Integer)] -> (Integer,Integer) -> Integer -> (Integer,Integer)
diveAim [] acc _ = acc
diveAim ((x,y):xs) (a,b) c | x == "forward" = diveAim xs (a+y,b+(y*c)) c
                           | x == "up" = diveAim xs (a,b) (c-y)
                           | x == "down" = diveAim xs (a,b) (c+y)