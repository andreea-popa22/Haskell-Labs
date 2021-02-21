import Test.QuickCheck
import Data.Char
import Test.QuickCheck.Gen (Gen(MkGen, unGen))

double :: Int -> Int 
double x = x*2

triple :: Int -> Int 
triple x = x*3

penta :: Int -> Int 
penta x = x*5

test :: Int -> Bool
test x = (double x + triple x) == (penta x)

--6
test1 :: Int -> Bool 
test1 x = (double x * triple x) == (penta x)

--7
myLookUp :: Int -> [(Int, String)] -> Maybe String 
myLookUp n [] = Nothing 
myLookUp n (x:xs)
    | fst x == n = Just (snd x)
    | otherwise  = myLookUp n xs

testLookUp :: Int -> [(Int,String)] -> Bool 
testLookUp n l = myLookUp n l == lookup n l --OK, passed 100 tests.

--Quickcheck cu constrangeri
testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list -- Gave up! Passed only 65 tests; 1000 discarded tests.

--8
--a)
capL :: (Int,String) -> String 
capL x = toUpper(head (snd x)):tail(snd x)

myLookUp' :: Int -> [(Int, String)] -> Maybe String 
myLookUp' n [] = Nothing 
myLookUp' n (x:xs)
    | fst x == n = Just (capL x)
    | otherwise  = myLookUp' n xs

--b)
testLookUp' :: Int -> [(Int,String)] -> Bool 
testLookUp' n l = myLookUp' n l == lookup n l

testLookUpCap :: Int -> [(Int, String)] -> Property  -- OK, passed 100 tests; 783 discarded.
testLookUpCap n list = list == (map(\(x,y) -> (x, cap y)) list) ==> testLookUp' n list
cap :: String -> String
cap sir = toUpper (head sir) : tail sir

--
data ElemIS = I Int | S String 
    deriving (Show,Eq)

instance Arbitrary ElemIS where 
    arbitrary = oneof [geni, gens]
        where 
            f = (unGen (arbitrary :: Gen Int))
            g = (unGen (arbitrary :: Gen String))
            geni = MkGen (\s i -> let x = f s i in (I x))
            gens = MkGen (\s i -> let x = g s i in (S x))

--9
myLookUpElem :: Int -> [(Int,ElemIS)] -> Maybe ElemIS
myLookUpElem n [] = Nothing 
myLookUpElem n ((c,e):xs)
    | c == n  = Just e
    | otherwise = myLookUpElem n xs

testLookUpElem :: Int -> [(Int,String)] -> Bool 
testLookUpElem n l = myLookUp n l == lookup n l  --OK, passed 100 tests.

