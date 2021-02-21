import Data.Char
import Data.List

fibonacciCazuri :: Integer -> Integer 
fibonacciCazuri n
    | n < 2 = n
    | otherwise = fibonacciCazuri(n-1) + fibonacciCazuri(n-2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational(n-1) + fibonacciEcuational(n-2)

fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where
    fibonacciPereche :: Integer -> (Integer, Integer)
    fibonacciPereche 1 = (0, 1)
    fibonacciPereche n = (b, a+b)
            where
                (a,b) = fibonacciPereche (n-1)

semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l
  | null l    = l
  | even h    = h `div` 2 : t'
  | otherwise = t'
  where
    h = head l
    t = tail l
    t' = semiPareRecDestr t

semiPareRecEq :: [Int] -> [Int]
semiPareRecEq [] = []
semiPareRecEq (h:t)
  | even h    = h `div` 2 : t'
  | otherwise = t'
  where t' = semiPareRecEq t

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

--L2.2
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec lo hi [] = []
inIntervalRec lo hi (h:t)
  | lo <= h && h <= hi = h:t'
  | otherwise     = t'
  where t' = inIntervalRec lo hi t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp lo hi xs = [x | x <- xs, lo <= x && x <= hi]

--L2.3
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) 
  | x > 0 = 1 + pozitiveRec xs
  | otherwise = pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp l = length([x | x <- l, x > 0])

--L2.4
--pozitiiAux :: [Int] -> Int -> Int
-- pozitiiAux (x:l) 0 = x
-- pozitiiAux [] ind = 0
-- pozitiiAux (x:l) ind = pozitiiAux l (ind-1) --returneaza elementul de pe o pozitie data
-- pozitiiAux [] x = -1
-- pozitiiAux (h:l) x 
--   | h == x  = 0
--   | otherwise = 1 + pozitiiAux l x   --returneaza indicele primei aparitii al unui element dat
-- pozitiiImpareRec :: [Int] -> [Int]
-- pozitiiImpareRec [] = []
-- pozitiiImpareRec l
--   | even x  = t
--   | otherwise = [ind] ++ t
--   where
--     x = head l
--     xs = tail l
--     t = pozitiiImpareRec xs
--     ind = pozitiiAux l x

pozitiiImpareRec::[Int]->[Int]
pozitiiImpareRec []=[]
pozitiiImpareRec l=  pozitiiImpareAux l 0
    where 
        pozitiiImpareAux::[Int]->Int->[Int]
        pozitiiImpareAux [] i=[]
        pozitiiImpareAux (h:t) i
            |odd h= i:pozitiiImpareAux t (i+1)
            |otherwise=pozitiiImpareAux t (i+1)

-- pozitiiImpareRec :: [Int] -> [Int]
-- pozitiiImpareRec l = pozitiiAux 0 1
--     where
--         pozitiiAux :: Int -> [Int] -> [Int]
--         pozitiiAux n(x:t) = let 
--             t' = pozitiiAux t
--             in (if (odd x) then n:t' else t')

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [y | (x,y) <- zip l [0..], odd x]

--L2.5
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) 
  | isDigit x = (digitToInt x) * multDigitsRec xs
  | otherwise = multDigitsRec xs

multDigitsComp :: String -> Int
multDigitsComp sir = product [ digitToInt x | x <- sir, isDigit x]

--L2.6
discountRec :: [Float] -> [Float]
discountRec [] = []
discountRec (h:l) 
  | red < 200 = red: discountRec l
  | otherwise = discountRec l
  where
    red = h - 0.25*h

discountComp :: [Float] -> [Float]
discountComp list = [ x-0.25*x | x <- list, x-0.25*x < 200]