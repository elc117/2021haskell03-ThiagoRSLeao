add10toall :: [Int] -> [Int]
add10toall listInt = map (\ i -> i + 10) listInt 

multN :: Int -> [Int] -> [Int]
multN n listInt = [ x * n | x <- listInt ]

multN' :: Int -> [Int] -> [Int]
multN' n listInt = map (\ x -> x * n) listInt

applyExpr :: [Int] -> [Int]
applyExpr listInt = [(x*3)+2 | x <- listInt]

applyExpr' :: [Int] -> [Int]
applyExpr' listInt = map (\x -> (x*3)+2) listInt

addSuffix :: String -> [String] -> [String]
addSuffix sufixo listStr = [sufixo++str | str <- listStr]

selectgt5 :: [Int] -> [Int]
selectgt5 listInt = [x | x <- listInt, x > 5]

sumOdds :: [Int] -> Int
sumOdds listInt = foldl1 (\ x y -> x+y) [x | x <- listInt, mod x 2 == 1]

sumOdds' :: [Int] -> Int
sumOdds' listInt = foldl1 (\ x y -> x+y) (filter (\ x -> if(mod x 2 == 1) then True
else False) listInt)

selectExpr :: [Int] -> [Int]
selectExpr listInt = [x | x <- listInt, x > 20, x < 50,  mod x 2 == 0]

countShorts :: [String] -> Int
countShorts listStr = length [str | str <- listStr, length str < 5]

calcExpr :: [Float] -> [Float]
calcExpr listFloat = [(x^2)/2 | x <- listFloat, (x^2)/2 > 10]

trSpaces :: String -> String
trSpaces str = [if(x == ' ') then '-' 
else x | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd a = [snd x | x <- a]

dotProd :: [Int] -> [Int] -> Int
dotProd listA listB =  foldl1 (\x y -> x + y) [fst x * snd x | x <- zip listA listB]
