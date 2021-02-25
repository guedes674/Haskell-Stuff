-- Exercicio 1

-- A
perimetro :: Double -> Double
perimetro r = 2*pi*r

-- B
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt(x2-x1)^2+(y2-y1)^2

-- C
primUlt :: [Int] -> (Int,Int)
primUlt l = (head l,last l)

--D
multiplo :: Int -> Int -> Bool
multiplo m n = (mod m n) == 0

--E
truncaImpar :: [Int] -> [Int]
truncaImpar x = do
    let y = length x
    if (mod y 2) > 0
        then tail x
        else x

--F
max2 :: Int -> Int -> Int
max2 a b = max a b

--G
max3 :: Int -> Int -> Int
max3 max2 z = max max2 z 

--Exercicio 2

--A
nraizes :: Double -> Double -> Double -> Int 
nraizes a b c = do
    if (determ) > 0
    then 2
    else if (determ) == 0
        then 1
        else 0
        where determ = b^2 - 4*a*c

--Ou

nraizes2 a b c 
    | z > 0 = 2
    | z == 0 = 1
    | otherwise = 0
    where z = b^2 - 4*a*c


--B
raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | nraizes a b c == 2 = [(-b+determi)/2*a,(-b-determi)/2*a]
    | nraizes a b c == 1 = [-b/2*a]
    | otherwise = []
    where determi = sqrt (b^2 - 4*a*c)

--Exercicio 3
type Hora = (Int,Int)

--A
valida :: Hora -> Bool
valida (h,m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

--B
depois :: Hora -> Hora -> Bool
depois (h1,m1) (h2,m2) = (h1 > h2) || (h1 == h2) && (m1 > m2)

--C
converterh2m :: Hora -> Int
converterh2m (h,m) = h*60 + m

--D
converterm2h :: Int -> Hora
converterm2h m = (div m 60,mod m 60)

--E
difh :: Hora -> Hora -> Int
difh h1 h2 = abs(m1 - m2)
    where
    m1 = converterh2m h1
    m2 = converterh2m h2

--F
adicionarmin :: Hora -> Int -> Hora
adicionarmin h1 m2 = converterm2h (m2 + (converterh2m h1))

--Exercicio 4
data Hora2 = H Int Int deriving (Show,Eq)

--A
valida2 :: Hora2 -> Bool
valida2 (H h m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

--B
depois2 :: Hora2 -> Hora2 -> Bool
depois2 (H h1 m1) (H h2 m2) = (h1 > h2) || (h1 == h2) && (m1 > m2)

--C
converterh2m2 :: Hora2 -> Int
converterh2m2 (H h m) = h*60 + m

--D
converterm2h2 :: Int -> Hora2
converterm2h2 m = (H (div m 60) (mod m 60))

--E
--difh2 :: Hora2 -> Hora2 -> Int
--difh2 (H h1 m1) (H h2 m2) = abs(m1 - m2)
--    where
--    m1 = converterh2m2 (H h1 m1)
--    m2 = converterh2m2 (H h2 m2)

--F
adicionarmin2 :: Hora2 -> Int -> Hora2
adicionarmin2 (H h1 m1) m2 = converterm2h2 (m2 + (converterh2m2 (H h1 m1)))

--Exercicio 5

--A
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
next :: Semaforo -> Semaforo
next Verde = Amarelo 
next Amarelo = Vermelho
next Vermelho = Verde

--B
stop :: Semaforo -> Bool
stop s |s == Verde = False
       |s == Amarelo = False
       |s == Vermelho = True
       |otherwise = False

--C
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = 
    if s1 == s2
        then False
    else True

--Exercicio 6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
--A
posx :: Ponto -> Double
posx p = case p of
    (Cartesiano x y) -> x
    (Polar dist ang) -> dist*cos ang

--B
posy :: Ponto -> Double
posy p = case p of
    (Cartesiano x y) -> y
    (Polar dist ang) -> dist*sin ang

--C
raio :: Ponto -> Double
raio p = case p of
    (Cartesiano x y) -> sqrt (x^2 + y^2)
    (Polar dist ang) -> dist

--D
angulo :: Ponto -> Double
angulo p = case p of
    (Cartesiano x y) -> atan (y / x)
    (Polar dist ang) -> ang

--E
dist1 :: Ponto -> Ponto -> Double
dist1 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

dist2 :: Ponto -> Ponto -> Double
dist2 a b = sqrt ((x2-x1)^2 + (y2-y1)^2)
    where
         x1 = posx a
         x2 = posx b
         y1 = posy a
         y2 = posy b

--Exercicio 7
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

--A
poligono :: Figura -> Bool
poligono p = case p of
    (Circulo _ _) -> False
    (Retangulo _ _) -> True
    (Triangulo _ _ _) -> True

--B
vertices :: Figura -> [Ponto]
vertices p = case p of
    (Circulo _ _ ) -> []
    (Triangulo p1 p2 p3) -> [p1,p2,p3]
    (Retangulo p1 p2) -> [p1,p2,p3,p4]
        where
            p3 = (Cartesiano (posx p1) (posy p2))
            p4 = (Cartesiano (posx p2) (posy p1))

--C
--area :: Figura -> Double
--area p = case p of
--    (Circulo m r) -> pi*r^2
--    (Retangulo p1 p2) -> 
--        let p3 = (Cartesiano (posx p1) (posy p2))
--            base = dist p2 p3
--            altura = dist p1 p3
--        in base * altura 
--    (Triangulo p1 p2 p3) ->
--        let a = dist p1 p2
--            b = dist p2 p3
--            c = dist p3 p1
--            s = (a + b + c) / 2 
--        in sqrt (s * (s - a) * (s - b) * (s - c))

--Exercicio 8
ord :: Char -> Int

--A
isLower :: Char -> Bool
isLower 
