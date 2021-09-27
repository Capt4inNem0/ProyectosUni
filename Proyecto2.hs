-- Ejercicio 1
-- 1-a y 2-a
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq, Ord)
-- 1-b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computación"
titulo Astronomia = "Licenciatura en Astronomia"
-- 2-a
ordenable :: Carrera -> Carrera -> Bool
ordenable x y = x <= y
-- Ejercicio 2
-- 2-a
ordenable' :: Carrera -> Carrera -> Carrera
ordenable' x y = min x y
-- Ejercicio 3
-- 3-a
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq, Show)

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Show)

-- 3-b El tipo de constructor de Docente es Cargo

-- 3-c
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc ((Docente x):xs) c  | x == c = 1 + cuantos_doc xs c
                                | otherwise = cuantos_doc xs c
cuantos_doc (x:xs) c = cuantos_doc xs c

-- 3-d
esdocenteytienecargo :: Cargo -> Persona -> Bool
esdocenteytienecargo c (Docente x) = x == c
esdocenteytienecargo c Decane = False
esdocenteytienecargo c (NoDocente x) = False
esdocenteytienecargo c (Estudiante a x) = False

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' xs c = length (filter (esdocenteytienecargo c) xs)

-- Ejercicio 4
-- 4-a
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x
-- Ejercicio 5
data Cola = VaciaC | Encolada Persona Cola deriving Show
-- 5 a 1
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada x xs) = Just xs
-- 5 a 2
encolar :: Persona -> Cola -> Cola
encolar y VaciaC = (Encolada y VaciaC)
encolar y x = Encolada y x
-- 5 a 3
busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC y = Nothing
busca (Encolada (Docente c) xs) y   | c == y = Just (Docente c)
                                    | otherwise = busca xs y
busca (Encolada Decane xs) y = busca xs y
busca (Encolada (NoDocente a) xs) y = busca xs y
busca (Encolada (Estudiante c i) xs) y = busca xs y

-- 5 b
-- Se parece mucho al tipo Entero, donde cada persona esta ordenada en una posicion y cuando no hay personas, es vacio, comparable con el 0

-- Ejercicio 6
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Eq, Show)
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

type GuiaTelefonica = ListaAsoc Int String

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b xs) = 1 + la_long xs

la_reversa :: ListaAsoc a b -> ListaAsoc a b
la_reversa Vacia = Vacia
la_reversa (Nodo a b xs) = (Nodo a b Vacia) 

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia xs = xs
la_concat (Nodo a b xs) ys = (Nodo a b (la_concat xs ys))

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia a = Nothing
la_busca (Nodo x y xs) a    | a == x = Just y
                            | otherwise = la_busca xs a

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar a Vacia = Vacia
la_borrar a (Nodo x y xs)   | a == x = la_borrar a xs
                                | otherwise = (Nodo x y (la_borrar a xs))


