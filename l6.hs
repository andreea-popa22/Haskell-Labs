--1
data Fruct
    = Mar String Bool 
    | Portocala String Int 

ePortocalaDeSicilia :: Fruct -> Bool 
ePortocalaDeSicilia (Portocala soi _)
    | soi == "Tarocco"  = True 
    | soi == "Moro"     = True 
    | soi == "Sanguinello" = True 
    | otherwise         = False 
ePortocalaDeSicilia _ = False

nrFeliiSicilia :: [Fruct] -> Int 
nrFeliiSicilia ((Mar _ _) : xs) = nrFeliiSicilia xs 
nrFeliiSicilia [] = 0
nrFeliiSicilia ((Portocala soi felii) : xs)
    | ePortocalaDeSicilia (Portocala soi felii)     = felii + nrFeliiSicilia xs
    | otherwise = nrFeliiSicilia xs 

nrMereViermi :: [Fruct] -> Int 
nrMereViermi ((Portocala _ _) : xs) = nrMereViermi xs
nrMereViermi [] = 0
nrMereViermi ((Mar _ v) : xs)
    | v == True     = 1 + nrMereViermi xs
    | otherwise     = nrMereViermi xs

--2
data Linie = L [Int]
    deriving Show 
data Matrice = M [Linie]

verifica :: Matrice -> Int -> Bool
verifica (M l) n = foldr (&&) True [sum x == n | (L x) <- l]

showLinie :: Linie -> [Char]
showLinie (L []) = ""
showLinie (L (x:xs)) = show(x) ++ " " ++ showLinie (L xs)

instance Show Matrice where
    show (M []) = ""
    show (M (x:[])) = (showLinie x)
    show (M (x:xs)) = (showLinie x) ++ "\n" ++ (show(M xs))

verificaLinie :: Linie -> Bool 
verificaLinie (L l) = foldr (&&) True [x > 0 | x <- l]

doarPozN :: Matrice -> Int -> Bool 
doarPozN (M l) n = foldr (&&) True [verificaLinie (L x) | (L x) <- l, length x == n]