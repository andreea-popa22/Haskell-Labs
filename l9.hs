import           Prelude hiding (lookup)
import qualified Data.List as List
data Expr 
    = Const Int 
    | Expr :+: Expr 
    | Expr :*: Expr 
    deriving Eq 

data Operation = Add | Mult 
    deriving (Eq, Show)

data Tree 
    = Lf Int 
    | Node Operation Tree Tree 
    deriving (Eq, Show)

--1.1
par :: String -> String 
par s = "(" ++ s ++ ")"

instance Show Expr where
    show (Const x) = show x
    show (exp1 :+: exp2) = par (show exp1 ++ " + " ++ show exp2)
    show (exp1 :*: exp2) = par (show exp1 ++ " * " ++ show exp2)

--1.2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

--1.3
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2

--1.4
expToArb :: Expr -> Tree 
expToArb (Const x) = Lf x 
expToArb (left :+: right) = Node Add (expToArb left) (expToArb right)
expToArb (left :*: right) = Node Mult (expToArb left) (expToArb right)

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

--1.5
class MySmallCheck a where
    smallValues :: [a]
    smallCheck :: ( a -> Bool ) -> Bool
    smallCheck prop = and [ prop x | x <- smallValues ]

instance MySmallCheck Expr where
    smallValues = [exp1, exp2, exp3, exp4]

--1.6
checkExp :: Expr -> Bool 
checkExp e = evalExp e == evalArb ( expToArb e )

test = foldr (&&) True [checkExp e | e <- smallValues]

--2
type Key = Int          -- Key este acum un sinonim in program pentru Int
type Value = String     -- Value este acum un sinonim in program pentru String 

class Collection c where 
    cempty :: c 
    csingleton :: Key -> Value -> c 
    cinsert :: Key -> Value -> c -> c 
    cdelete :: Key -> c -> c 
    clookup :: Key -> c -> Maybe Value 
    cToList :: c -> [(Key, Value)]

--2.1
    ckeys :: c -> [Key]
    ckeys c = [fst p | p <- cToList c]
    cvalues :: c -> [Value]
    cvalues c = [snd p | p <- cToList c]
    cFromList :: [(Key, Value)] -> c 
    cFromList [] = cempty 
    cFromList ((k, v):kvs) = cinsert k v (cFromList kvs)

--2.2
newtype PairList = PairList { getPairList :: [(Key, Value)] }

instance Show PairList where 
    show (PairList pairList) = "PairList " ++ show pairList 

instance Collection PairList where 
    cempty = PairList []
    csingleton k v = PairList [(k, v)]
    cinsert k v (PairList list) = if elem k (map fst list) then PairList list else PairList ((k, v) : list)
    cdelete k (PairList list) = PairList [(k1, v1) | (k1, v1) <- list, k1 /= k]
    clookup k (PairList list) = lookup k list
    cToList = getPairList

--2.3
data SearchTree 
    = Empty 
    | Node1 SearchTree Key (Maybe Value) SearchTree 
    deriving Show 

instance Collection SearchTree where 
    cempty = Empty 
    csingleton k v = Node1 Empty k (Just v) Empty 
    clookup k Empty = Nothing 
    clookup k (Node1 treeLeft k1 v1 treeRight)
        | k == k1 = v1 
        | k < k1 = clookup k treeLeft 
        | otherwise = clookup k treeRight 
    cinsert k v Empty = csingleton k v
    cinsert k v (Node1 lefttree key value righttree)
        | k < key = Node1 (cinsert k v lefttree) key value righttree
        | k > key = Node1 lefttree key value (cinsert k v righttree)
        | otherwise = Node1 lefttree key value righttree
    cdelete k v = go where
        go Empty= Empty
        go (Node1 arbst key val  arbdr)
            | k==key = Node1 arbst k (Nothing) arbdr
            | k < key = Node1 (go arbst) key val arbdr
            | otherwise = Node1 arbst key val (go arbdr)
    ctoList (Node1 arbst key (Just val) arbdr) = (ctoList arbst) ++ [(key, val)] ++(ctoList arbdr)
    -- clookup k = go where
    --     go Empty = Nothing
    --     go (Node1 arbst key val arbdr)
    --         | k == key = val
    --         | k <key = go arbst
    --         | otherwise = go arbdr
-------------------------------------------------------
-- Functorii 

data Arbore a
    = Frunza a
    | Nod a (Arbore a) (Arbore a) 
    deriving (Eq, Show)

-- vreau sa fac o instanta a clasei Functor pentru tipul de date Arbore 
-- primind o functie (a -> b), un Arbore a => Arbore b 

instance Functor Arbore where 
    fmap f (Frunza x) = Frunza (f x)
    fmap f (Nod x left right) = Nod (f x) (fmap f left) (fmap f right)

-- instance Functor Arbore where
--     fmap f (Frunza a) = Nod Vid (f a) Vid
--     fmap f (Nod arb1 a arb2) = Nod (fmap f arb1) (f a) (fmap f arb2)

-- instance Foldable Arbore where
--     foldr f acc Vid = acc
--     foldr f acc (Frunza a) = f a (foldr f acc Vid)
--     foldr f acc (Nod arb1 x arb2) = foldr f (f x (foldr f acc arb2)) arb1

-- pot utiliza <$> 

arbore = Nod 2 (Nod 3 (Frunza 2) (Frunza 3)) (Frunza 5)

-- instance Functor Maybe where 
--     fmap f Nothing = Nothing 
--     fmap f (Just x) = Just (f x)