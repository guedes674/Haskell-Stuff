--1
eft :: Int -> Int -> [Int]
eft x y | x == y = [x]
        | x < y = x:(eft (x+1) y)
        | otherwise = []

--2
eftt :: Int -> Int -> Int -> [Int]
eftt x y z | x == z = [x]
           | x < z = x : eftt (x + (y-1)) y z
           | otherwise = []

--3
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena (x1:y1) [] = (x1:y1)
concatena [] (x1:y1) = (x1:y1)
concatena (x1:y1) (x2:y2) = x1:concatena y1 (x2:y2)

--4
posicao :: [a] -> Int -> a
posicao (h:t) a = if a == 0
                  then h
                  else posicao t (a - 1)

--5
reversa :: [a] -> [a]
reversa [] = []
reversa (h:t) = reversa t++[h]

--6
pegar :: Int -> [a] -> [a]
pegar _ [] = []
pegar 0 l = []
pegar a (h:t) = h:(pegar (a-1) t)

--7
deitar :: Int -> [a] -> [a]
deitar _ [] = []
deitar 0 (h:t) = (h:t)
deitar a (h:t) = deitar (a-1) t

--8
fecho :: [a] -> [b] -> [(a,b)]
fecho [] [] = []
fecho _ [] = []
fecho [] _ = []
fecho (h1:t1) (h2:t2) = (h1,h2):(fecho t1 t2)

--9
elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
elemento x (h:t) = if x == h
                   then True
                   else elemento x t

--10
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar x n = n : if x == 0
                   then [n]
                   else replicar (x-1) n

--11
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [h] = [h]
intersperse e (h:t) = (h:e:(intersperse e t))

--12
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (h:t) = takeWhile (==h) (h:t) : agrupar (dropWhile (==h) (h:t))

--13
concatena2 :: [[a]] -> [a]
concatena2 [] = []
concatena2 (h:t) = h++concatena2 t

--14
prefixos :: [a] -> [[a]]
prefixos [] = [[]]
prefixos l = prefixos (init l) ++ [l]

-- prefixos [11,21,13] = prefixos ([11,21]) ++ [[11,21,13]] = [[11,21],[11,21,13]]
-- prefixos [11,21]    = prefixos ([11]) ++ [[11,21],[11,21,13]] = [[11],[11,21],[11,21,13]]
-- prefixos [11]       = prefixos ([]) ++ [[11],[11,21],[11,21,13]] = [[],[11],[11,21],[11,21,13]]

--15
sufixos :: [a] -> [[a]]
sufixos [] = [[]]
sufixos (h:t) = (h:t) : (sufixos t)

--16
prefixoDe :: Eq a => [a] -> [a] -> Bool
prefixoDe [] [] = True
prefixoDe _ [] = False
prefixoDe [] _ = True
prefixoDe (h:t) (h1:t1) | h == h1 = prefixoDe t t1 
                        | otherwise = False

--17
sufixoDe :: Eq a => [a] -> [a] -> Bool
sufixoDe [] [] = True
sufixoDe _ [] = False
sufixoDe [] _ = True
sufixoDe l1 l2 | last l1 == last l2 = sufixoDe (init l1) (init l2)
               | otherwise = False

--18
ordemrltv :: Eq a => [a] -> [a] -> Bool
ordemrltv [] [] = True
ordemrltv [] _ = False
ordemrltv _ [] = False
ordemrltv (h1:t1) (h2:t2) | h1 == h2 = ordemrltv t1 t2
                          | otherwise = ordemrltv (h1:t1) t2

--19
elemInd :: Eq a => a -> [a] -> [Int]
elemInd _ [] = []
elemInd n (h:t) = elemIndAux n (h:t) 0

elemIndAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndAux _ [] _ = []
elemIndAux x (h:t) n = if x == h
                           then [n] ++ elemIndAux x t (n+1)
                       else elemIndAux x t (n+1)

--20
mesmos :: Eq a => [a] -> [a]
mesmos [] = []
mesmos (h:t) = if elem h t
                   then mesmos t
               else h:mesmos t

--21
apagar :: Eq a => a -> [a] -> [a]
apagar _ [] = []
apagar x (h:t) | x == h = t
               | otherwise = h:apagar x t

--22
removerprimeiros :: Eq a => [a] -> [a] -> [a]
removerprimeiros [] _ = []
removerprimeiros l [] = l
removerprimeiros (h1:t1) (h2:t2) | h2 == h1 = removerprimeiros t1 t2
                                 | otherwise = h1:removerprimeiros t1 (h2:t2)

--23
uniao :: Eq a => [a] -> [a] -> [a]
uniao l [] = l
uniao [] l = l
uniao (h1:t1) (h2:t2) | h1 == h2 = h1:uniao t1 t2
                      | otherwise = h1:uniao t1 (h2:t2)

--24
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao l [] = l
intersecao [] _ = []
intersecao (h1:t1) (h2:t2) | elem h1 (h2:t2) = h1: intersecao t1 (h2:t2)
                           | otherwise = intersecao t1 (h2:t2)

--25
inserir :: Ord a => a -> [a] -> [a]
inserir x [] = [x]
inserir x (h:t) | x > h = h:inserir x t
                | otherwise = x:h:t

--26
despalavrar :: [String] -> String
despalavrar [] = ""
despalavrar (h:t) = h ++ " " ++ despalavrar t

--27
desalinhar :: [String] -> String
desalinhar [] = ""
desalinhar (h:t) = h ++ "\n" ++ desalinhar t

--28
-- pdoMaior :: Ord a => [a] -> Int
-- pdoMaior [x] = 0
-- pdoMaior (h:t) | h == aux (h:t) = 0
--                | otherwise = 1 + pdoMaior t
--     where aux x = [x]
--           aux (x:y:xs) | x > y = (x:xs)
--                        | otherwise = (y:xs)

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) | elem h t = True
                   | otherwise = temRepetidos t

--30
algarismos :: [Char] -> [Char]
algarismos [] = ""
algarismos (h:t) | h >= '1' && h <= '9' = h:algarismos t
                 | otherwise = algarismos t

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (h:x:t) = x:posImpares t

--32
posPares :: [a] -> [a]
posPares [] = []
posPares (h:x:t) = h:posPares t

--33
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada (h:x:t) | h <= x = estaOrdenada (x:t)
                     | otherwise = False

--34
minhaOrdenada :: Ord a => [a] -> [a]
minhaOrdenada [] = []
minhaOrdenada (h:t) = auxiliar h (minhaOrdenada t)

auxiliar :: Ord a => a -> [a] -> [a]
auxiliar x [] = [x]
auxiliar x (h:t) | x <= h = (x:h:t)
                 | otherwise = h:auxiliar x t

--35
menor' :: String -> String -> Bool
menor' [] [] = True
menor' _ [] = False
menor' [] _ = True
menor' (h1:t1) (h2:t2) | h1 < h2 = True
                       | h1 > h2 = False
                       | otherwise = menor' t1 t2

--36
pertenceaoC :: Eq a => a -> [(a,Int)] -> Bool
pertenceaoC _ [] = False
pertenceaoC x (h:t) | x == fst h = True
                    | otherwise = pertenceaoC x t

--37
tamanhoMC :: [(a,Int)] -> Int
tamanhoMC [] = 0
tamanhoMC ((a,b):t) = b + tamanhoMC t

--38
converteremL :: [(a,Int)] -> [a]
converteremL [] = []
converteremL ((a,1):t) = a:converteremL t
converteremL ((a,b):t) = [a] ++ converteremL ((a,b-1):t)

--39
insereemMC :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereemMC _ [] = []
insereemMC x ((a,b):t) | x == a = ((a,b+1):t)
                       | otherwise = (a,b):insereemMC x t
--40
removeaMC :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeaMC _ [] = []
removeaMC x ((a,1):t) | x == a = t
                      | otherwise = (a,1):removeaMC x t
removeaMC x ((a,b):t) | x == a = ((a,b-1):t)
                      | otherwise = (a,b):removeaMC x t

--41
--constroiMset1 :: Ord a => [a] -> [(a,Int)]

-- constroiMset [1,1,2,3,3,4,5,6,6,6,7] 
--     -> [(1,2),(2,1),(3,2),(4,1),(5,1),(6,3),(7,1)]

-- constroiMset1 l -> reorganiza (group l)
--     where reorganiza :: [[a]] -> [(a,Int)]
--           reorganiza [] -> []
--           reorganiza (h:t) -> (head h, length h): reorganiza t

constroiMset2 :: Ord a => [a] -> [(a,Int)]
constroiMset2 [] = []
constroiMset2 (h:t) = adiciona h (constroiMset2 t)
    where adiciona x [] = [(x,1)]
          adiciona x ((y,n):ys) | x == y = (y,n+1):ys
                                | x /= y = (y,n):adiciona x ys

--42
partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 (e:es) = case e of
                                Left a -> ((a:partitionEithers1 es),y)
                                Right b -> (x,(b:y))
    where (x,y) = (partitionEithers1 es)

--43
catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 ((Just x):xs) = x : catMaybes1 xs
catMaybes1 ((Nothing):xs) = catMaybes1 xs

--44
data Movimento = Norte | Sul | Este | Oeste deriving (Show,Eq)

posicaoRobot :: (Int,Int) -> [Movimento] -> (Int,Int)
posicaoRobot (0,0) [] = (0,0)
posicaoRobot (x,y) [] = (x,y)
posicaoRobot (x,y) (d1:d2) | d1 == Norte = posicaoRobot (x,y + 1) d2
                           | d1 == Sul = posicaoRobot (x,y - 1) d2
                           | d1 == Este = posicaoRobot (x - 1,y) d2
                           | d1 == Oeste = posicaoRobot (x + 1,y) d2
                           | otherwise = error "Nao ando"

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | (a == c) && (b == d) = []
                    | a > c = Oeste:caminho (a - 1,b) (c,d)
                    | a < c = Este:caminho (a + 1,b) (c,d)
                    | b > d = Sul:caminho (a,b - 1) (c,d)
                    | otherwise = Norte:caminho (a,b + 1) (c,d)

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (d1:d2) | d1 == Norte = vertical d2
                 | d1 == Sul = vertical d2
                 | otherwise = False

--47
data Posicao = Pos Int Int deriving Show

maisCentral:: [Posicao]-> Posicao
maisCentral [x] = x 
maisCentral ((Pos a b):(Pos c d):t) | dist (a,b) > dist (c,d) = maisCentral ((Pos c d):t)
                                    | otherwise = maisCentral ((Pos a b):t)

dist :: (Int,Int) -> Float
dist (x,y)= sqrt (fromIntegral ((x^2) + (y^2)))

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y) ((Pos w z):xs) | (x == (w-1)) || (x == (w +1)) || (y == (z-1)) || (y== (z+1)) = (Pos w z ) : (vizinhos (Pos x y) xs)
                                  | otherwise = vizinhos (Pos x y) xs

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos a b):(Pos c d):t) | b == d = mesmaOrdenada ((Pos a b):t)
                                      | otherwise = False

--50
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l | (aux1 l) > 1 = False
                | otherwise = True

aux1 :: [Semaforo] -> Int
aux1 [] = 0
aux1 (h:t) | h == Verde = 1 + aux1 t
           | h == Amarelo = 1 + aux1 t
           | otherwise = aux1 t
