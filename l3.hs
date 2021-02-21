import Data.List

--L3.1
--xs = [x^2 | x <- [1..10], x `rem` 3 == 2]
--xs = [(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
--xs = [(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
--xs = [x | x <- "Facultatea de Matematica si Informatica", elem x ['A' .. 'Z']]
--xs = [[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]

--1
factori :: Int -> [Int]
factori x = [d | d <- [1..x], rem x d == 0]

--2
prim :: Int -> Bool
prim x
    | x <=2 =True 
    | length ( factori x ) == 2 = True 
    | otherwise = False

--3
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x == True]

-- L3.2
-- [(x,y) | x <- [1..5], y <- [1..3]]
-- zip [1..5] [1..3]
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 (a:as) (b:bs) (c:cs) = (a,b,c) : myzip3 as bs cs
myzip3 _ _ _ = []

--L3.3
--xs = map (\ x -> 2 * x) [1 .. 10]
--xs = map (1 `elem` ) [[2, 3], [1, 2]]
--xs = map ( `elem` [2, 3] ) [1, 3, 4, 5]

--1
firstEl :: [(a,b)] -> [a]
firstEl l = map fct l
    where fct (a,b) = a

--2
sumList :: [[Int]] -> [Int]
sumList l = map fct l
    where fct ls = sum ls

--3
fct :: Int -> Int
fct x 
    | even x    = div x 2
    | otherwise = x*2
prel2 :: [Int] -> [Int]
prel2 l = map fct l

--L3.4
--1
f1 :: Char -> [String] -> [String]
f1 c l = filter (elem c) l

--2
f2 :: [Int] -> [Int]
f2 l = map (^2) (filter odd l)

--3
f3 :: [Int] -> [Int]
f3 l = map (\(x,y) -> x*x) (filter (\(x,y) -> odd y) (zip l [1..]))

--4
f4 :: [String] -> [String]
f4 l = map (\s -> filter (\c -> elem c "aeiouAEIOU") s) l

--L3.5
mymap :: (a -> b) -> [a] -> [b]
mymap f l = [f x | x <- l]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p l = [x | x <- l, p x]