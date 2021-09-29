data LugarALimpiar = Cocina | Habitación | Comedor | Baño deriving (Eq, Show)
type Fiaca = Int

cantidadDeFiaca :: LugarALimpiar -> Fiaca
cantidadDeFiaca Cocina = 4
cantidadDeFiaca Habitación = 0
cantidadDeFiaca Comedor = 2
cantidadDeFiaca Baño = 3


type NombrePersona = String
data HayQueLimpiar = Ninguna | AgregaTarea LugarALimpiar NombrePersona HayQueLimpiar

tocaLimpiar :: HayQueLimpiar -> LugarALimpiar -> NombrePersona -> Bool
tocaLimpiar Ninguna l p = False
tocaLimpiar (AgregaTarea xl xp xs) l p = (xl == l && xp == p) || tocaLimpiar xs l p



data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Eq, Show)
type Tiempo = Int

agregaLA :: ListaAsoc LugarALimpiar Tiempo -> LugarALimpiar -> Tiempo -> ListaAsoc LugarALimpiar Tiempo
agregaLA lista l t = Nodo l t lista


data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a )
aMenor :: Arbol Int -> Int -> Bool
aMenor Hoja i = False
aMenor (Rama xl x xr) i = (aMenor xl i) || x<i || (aMenor xr i)
