{-
Exercicio 1

A
funA :: [Double] -> Double
funA (2:3,5,1) = 2^2 + funA [3,5,1]
                 4 + funA [3:5,1]

                 = 4 + 3^2 + funA [5,1]
                 13 + funA [5:1]

                 = 13 + 5^2 + funA [1]
                 38 + funA [1:[]]

                 = 38 + 1^2 + funA []
                 39

B
funB :: [Int] -> [Int]
funB [] = []
funB(h:t) = if (mod h 2) == 0 then h : (funB t)
                              else (funB t)

funB (8:5,12) => if (mod 8 2) == 0
                 then 8 : (funB [5,12])
                 else funB [5,12] = 8 funB [5,12]
                 8 : if (mod 5 2) == 0
                 	then 5 funB [12]
                 	else funB [12]

                 8 : if (1) == 0
                 	then 5 : funB [12]
                 	else funB [12]

                 8 : funB [12]

                 8 : if (mod 12 2) == 0
                 	then 12 funB []
                 	else funB []

                 8 : 12 : funB []

                 8 : 12 : []

-}

import Data.Char

--Exercicio 2

--A
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2 * h : (dobros t)

--B
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t) = if (c == h)
                        then 1 + (numOcorre c t)
                      else (numOcorre c t)

--C
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = (h > 0) && (positivos t)

--D
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) =
    if (h > 0)
        then h : (soPos t)
        else soPos t

--E
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h < 0)
                    then h + (somaNeg t)
                    else somaNeg t

--F
tresUlt :: [a] -> [a]
tresUlt (h:t) = 
    if (length (h:t) <= 3)
        then (h:t)
    else tresUlt t

--G
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a1,b1):xs) = b1 : segundos xs

--H
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):xs) = if x == a
                            then True
                                else nosPrimeiros x xs

--I
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(x,y,z):xs) = sumTriplos ((a+x,b+y,c+z):xs)

--Exercicio 3

--A
--soDigitos :: [Char] -> [Char]
--soDigitos [] = []
--soDigitos (x:xs) = if (ord x) >= 48 && (ord x) <= 57
--                       then x : soDigitos xs
--                   else soDigitos xs

--B
--minusculas :: [Char] -> Int
--minusculas [] = 0
--minusculas (x:xs) = if (isLower x)
--                        then 1 + minusculas xs
--                    else minusculas xs

--C
--nums :: String -> [Int]
--nums [] = []
--nums (x:xs) = if (isDigit x)
--                  then (digit2Int x):(nums xs)
--              else nums xs

--Exercicio 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--A
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta e ((c,e'):xs) = if e == e'
                          then 1 + conta xs
                      else conta e xs

--B
grau :: Polinomio -> Int
grau [] = 0
grau ((c1,e1):(c2,e2):xs) = if e1 > e2
                                then grau ((c1,e1):xs)
                            else grau ((c2,e2):xs)
--C
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((c,e):xs) = if (g == e)
                           then (c,e):selgrau g xs
                       else selgrau g xs

--D
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ([c,1]) = []
deriv ((c,e):t) = (c*(fromIntegral g),(g-1)):deriv t

--E
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0.0
calcula 0 _ = 0.0
calcula x ((c,e):t) = (c*(x^e)) + calcula x t

--F
simp :: Polinomio -> Polinomio
simp [] = []
simp ([c,0]) = []
simp (x:t) = x : simp t

--G
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c1,g1) ((c2,g2):t) = (c1*c2,g1+g2) : mult (c1,g1) t

--H
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' [(b,e)] = [(b,e)]
normaliza' ((b1,e1):(b2,e2):t) | e == e2 = normaliza' ((b + b2, e) : ps)
                               | conta e ps == 0 = (b, e) : normaliza' ((b2, e2) : ps)
                               | otherwise = normaliza' ((b, e) : ps ++ [(b2, e2)

--I
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza' (p1 ++ p2)

--J
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p = soma (mult h p) (produto t p)

--K
ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena (h:t) = insereMon h (ordena t)

insereMon :: Monomio -> Polinomio -> Polinomio
insereMon x [] = [x]
insereMon (c,g) ((c1,g1):t) = if g >= g1 then (c,g) : ((c1,g1):t)
                              else (c1,g1) : insereMon (c,g) t

--L
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 | (ordena (normaliza' p1) == ordena (normaliza' p2)) = True
            | otherwise = False
