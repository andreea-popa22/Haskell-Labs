import Data.Maybe
import Data.List (nub)
type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

--1
p1 :: Prop 
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :|: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--2
par :: String -> String 
par s = "(" ++ s ++ ")"

instance Show Prop where
    show F = "F"
    show T = "T"
    show ( Var x ) = x
    show ( Not p ) = par ( "~" ++ show p )
    show ( p :|: q ) = par ( show p ++ "|" ++ show q )
    show ( p :&: q ) = par ( show p ++ "&" ++ show q )
    show ( p :->: q ) = par ( show p ++ "->" ++ show q )
    show ( p :<->: q ) = par ( show p ++ "<->" ++ show q )

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") ==  "((~P)&Q)"

----------------
type Env = [(Nume,Bool)]

impureLookUp :: Eq a => a -> [(a,b)] -> b
impureLookUp a = fromJust . lookup a

eval :: Prop -> Env -> Bool 
eval (Var p) env = impureLookUp p env
eval T _ = True 
eval F _ = False
eval (Not p) env = not (eval p env)
eval (p1 :&: p2) env = (eval p1 env) && (eval p2 env)
eval (p1 :|: p2) env = (eval p1 env) || (eval p2 env)
eval (p1 :->: p2) env = (not (eval p1 env)) || (eval p2 env)
eval (p1 :<->: p2) env = (eval (p1 :->: p2) env) && (eval (p2 :->: p1) env)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)]

--4
variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile T = []
variabile F = []
variabile (Not p) = variabile p
variabile (p :&: q) = nub (variabile p ++ variabile q)
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile (p :->: q) = nub (variabile p ++ variabile q)
variabile (p :<->: q) = nub (variabile p ++ variabile q)


test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--5
envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = [(x, False): e | e <- envs xs ] ++ [ ( x , True ) : e | e <- envs xs ]

test_envs =
    envs ["P", "Q"]
    ==
    [ [ ("P",False), ("Q",False)]
    , [ ("P",False), ("Q",True)]
    , [ ("P",True), ("Q",False)]
    , [ ("P",True), ("Q",True)]
    ]

--6
satisfiabila :: Prop -> Bool
satisfiabila prop = foldr (||) False [eval prop x | x <- envs ( variabile prop)]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True 
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--7
valida :: Prop -> Bool
valida prop = (foldr (&&) True [eval prop x | x <- envs ( variabile prop)]) == True

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--8
show_Bool :: Bool -> [Char]
show_Bool True = "T"
show_Bool False = "F"

tabelAdevar :: Prop -> String
tabelAdevar p = concat $ map (++ "\n") tabel
    where
    vars = variabile p
    afis_prima = concat $ (map (++ " ") vars) ++ [show p]
    evaluari = envs vars
    aux_af tv= (show_Bool tv) ++ " "
    afis_evaluari ev = concat $ (map aux_af [snd p | p <-ev]) ++ [show_Bool (eval p ev)]
    tabel = afis_prima : (map afis_evaluari evaluari)

--9
--putStrLn$ tabelAdevar (Var "P" :->: Var "Q")
--putStrLn$ tabelAdevar (Var "P" :<->: Var "Q")

--10
echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = valida ( p1 :<->: p2)
--echivalenta p1 p2 = foldr (&&) True [(eval p1 envir)==(eval p2 envir) | envir <- envs ( (variabile p1) ++ (variabile p2))]

test_echivalenta1 = True == (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))