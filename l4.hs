import Numeric.Natural

--1
produsRec :: [Int] -> Int
produsRec [] = 1
produsRec (x:xs) = x * produsRec xs

produsFold :: [Integer] -> Integer
produsFold l = foldr (*) 1 l

--2
andRec :: [Bool] -> Bool
andRec [] = True 
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold l = foldr (&&) True l

--3
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold l = foldr (++) [] l

--4
rmChar :: Char -> String -> String
rmChar c cuv = [x | x <- cuv, x /= c]

rmCharsRec :: String -> String -> String
rmCharsRec cs [] = []
rmCharsRec cs (x:xs) 
    | elem x cs     = rmCharsRec cs xs
    | otherwise     = x : rmCharsRec cs xs

rmCharsFold :: String -> String -> String
rmCharsFold cs cuv = foldr (:) [] (filter (\ x -> elem x cs == False) cuv) 

numerePrimeCiur :: Int -> [Int]
numerePrimeCiur n = 2 : eratos [3, 5..n]
    where
        eratos [] = []
        eratos (p:xs) 
            | p*p > n    = p : xs
            | otherwise  = p : eratos (filter (\x -> (elem x (map (p*) [p,p+2..n]) == False) ) xs) 

-- numerePrimeCiur ::Int->[Int]
-- numerePrimeCiur 1=[]
-- numerePrimeCiur 0=[]
-- numerePrimeCiur n=auxCiur [2..n]
--     where 
--         auxCiur::[Int]->[Int]
--         auxCiur []=[]
--         auxCiur (h:t)=h: auxCiur [x|x<-t ,x `mod`h>0]

---------------------------------------------------------------------------
--Functii de nivel inalt
--1
ordonataNat :: [Int] -> Bool 
ordonataNat [] = True 
ordonataNat [x] = True 
ordonataNat (x:xs) = and [ x < y | (x,y) <- zip (x:xs) xs ]

--2
ordonataNat1 :: [Int] -> Bool 
ordonataNat1 [] = True 
ordonataNat1 [x] = True
ordonataNat1 (x:xs) = x < head xs && ordonataNat1 xs 

--3 
ordonata :: [a] -> (a -> a -> Bool) -> Bool 
ordonata l p = and [ p x y | (x,y) <- zip l (tail l)]

ordine :: Int -> Int -> Bool 
ordine x y 
    | x < y     = True 
    | otherwise = False

divizibilitate :: Int -> Int -> Bool 
divizibilitate x y 
    | mod x y == 0  = True 
    | otherwise     = False

lexicografica :: [Char] -> [Char] -> Bool
lexicografica x y 
    | x < y     = True 
    | otherwise = False

(*<*) :: (Int, Int) -> (Int, Int) -> Bool
(*<*) (a, b) (c, d)
    | a < c && b < d = True
    | otherwise = False

--4
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList f l = map (f.) l

aplicaList :: a -> [(a -> b)] -> [b]
aplicaList x l = [f x | f <- l]  

myzip32 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip32 la lb lc = map (\ ((a, b), c) -> (a, b, c)) (zip (zip la lb) lc)