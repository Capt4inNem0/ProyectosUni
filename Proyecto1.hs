-- Ejercicio 1
-- a
esCero :: Int -> Bool
esCero x = x == 0
-- b
esPositivo :: Int -> Bool
esPositivo x = x > 0
-- c
esVocalb :: Char -> Bool
esVocalb x = x `elem` "aeiou"
-- Ejercicio 2
-- a
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs
-- b
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
-- c
productoria :: [Int] -> Int 
productoria [] = 1
productoria (x:xs) = x * productoria xs
-- d
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)
-- e
promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)
-- Ejercicio 3
pertenece :: Int -> [Int] -> Bool
pertenece y [] = False 
pertenece y (x:xs) = y == x || pertenece y xs
-- Ejercicio 4
-- a
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t
-- b
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t
-- c
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t
-- d
productoria' :: [a] -> (a ->Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t

-- Ejercicio 5
paraTodo'' :: [Bool] -> Bool
paraTodo'' xs = paratodo' xs id

-- Ejercicio 6
-- a
esPar :: Int -> Bool
esPar x = mod x 2 == 0
todoPares :: [Int] -> Bool
todoPares xs = paratodo' xs esPar
-- b
esMultiplo :: Int -> Int -> Bool
esMultiplo x y = mod y x == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs (esMultiplo y)
-- c
cuadrado :: Int -> Int
cuadrado n = n * n

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [1..n] cuadrado
-- d
factorialNoRecursivo :: Int -> Int
factorialNoRecursivo x = productoria [2..x]
-- e
imparesSon1 :: Int -> Int
imparesSon1 n   | esPar n = n
                | otherwise = 1

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs imparesSon1

--
-- Ejercicio 7
{- 
¿Que hace la funcion Map?
La funcion Map es una funcion de alto orden que toma una funcion y una lista de elementos y aplica esa funcion
a cada elemento de la lista. 
¿Que hace la funcion Filter?
La funcion Filter es una funcion de alto orden que toma una funcion que devuelve un booleano y una lista de elementos, 
devuelve una lista sin los elementos que al pasar por la funcion dada, retornaron False.

map succ [1, -4, 6, 2, -8] = [2, -3, 7, 3, -7]
filter esPositivo [1, -4, 6, 2, -8] = [1, 6, 2]
-}

--
-- Ejercicio 8
por2 :: Int -> Int
por2 n = n * 2
-- a
todosPor2 :: [Int] -> [Int]
todosPor2 [] = []
todosPor2 (x:xs) = por2 x : todosPor2 xs 
 -- b
todosPor2' :: [Int] -> [Int]
todosPor2' xs = map (por2) xs
--

--
-- Ejercicio 9
-- a
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs)    | esPar x = x : soloPares xs
                    | otherwise = soloPares xs
-- b
soloPares' :: [Int] -> [Int]
soloPares' xs = filter (esPar) xs
-- c
multiplicaParesMejorada :: [Int] -> Int
multiplicaParesMejorada xs = productoria (soloPares xs)

-- Ejercicio 10
-- a
primIgualesA :: Int -> [Int] -> [Int]
primIgualesA n [] = []
primIgualesA n (x:xs)   | n == x = x : primIgualesA n xs
                        | otherwise = []

primIgualesA' :: Int -> [Int] -> [Int]
primIgualesA' n xs = takeWhile (n ==) xs

--
-- Ejercicio 11
-- a
primIguales :: [Int] -> [Int]
primIguales [] = []
primIguales [x] = []
primIguales (y:x:xs)   | y == x = y : primIguales (x:xs)
                       | otherwise = [y]
-- b
primIguales' :: [Int] -> [Int]
primIguales' xs = primIgualesA' (head xs) xs

