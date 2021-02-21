--L1.4
myInt = 5
double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer 
maxim x y = 
    if (x>y) 
        then x 
        else y

maxim3 :: Integer -> Integer -> Integer -> Integer 
maxim3 x y z =
    let
        u = maxim x y
    in
        maxim u z

maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 x y z w =
    let 
        u = maxim x y
    in 
        let 
            v = maxim u z
        in 
            maxim v w

--L1.6
data Alegere
    = Piatra
    | Foarfeca
    | Hartie
    deriving (Eq, Show)

data Rezultat
    = Victorie
    | Infrangere
    | Egalitate
    deriving (Es, Show)

-- partida :: Alegere -> Alegere -> Rezultat
-- partida x y =