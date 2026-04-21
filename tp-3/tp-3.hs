{-1. Tipos recursivos simples
 1.1. Celdas con bolitas
 Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:-}
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
{- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría ser la siguiente:-}
bol1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
bol0 = CeldaVacia
--Implementar las siguientes funciones sobre celdas:

  -- 1)  Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas::Color->Celda->Int 
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita co ce) = unoSi (esMismoColor c co) + nroBolitas c ce
-- Precondición: no tiene

esMismoColor::Color->Color->Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False
-- Precondición: no tiene

unoSi::Bool->Int
unoSi True = 1
unoSi _ = 0
-- Precondición: no tiene

  -- 2) Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner::Color->Celda->Celda
poner co celda = Bolita co celda
-- Precondición: no tiene

  -- 3)  Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar::Color->Celda->Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita co ce) = if (esMismoColor c co)
                         then ce
                         else Bolita co (sacar c ce)
-- Precondición: no tiene

  -- 4)  Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN::Int->Color->Celda->Celda
ponerN n c celda = if (esMenorOIgualQueCero n)
                   then celda
                   else Bolita c (ponerN (n-1) c celda)
-- Precondicion: no tiene

esMenorOIgualQueCero::Int->Bool
esMenorOIgualQueCero n = n <= 0
-- Precondicion: no tiene

{- 1.2. Camino hacia el tesoro
 Tenemos los siguientes tipos de datos -}
data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

cam0 = Fin
cam1 = Nada Fin
cam2 = Cofre [] cam1
cam3 = Cofre [Cacharro] cam0
cam4 = Cofre [Tesoro] cam3
cam5 = Cofre [] cam4
cam6 = Nada cam5
cam7 = Cofre [Tesoro, Cacharro, Tesoro] cam6
-- Definir las siguientes funciones:

  -- 1)  Indica si hay un cofre con un tesoro en el camino.
hayTesoro::Camino->Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obs c) = tieneTesoro obs || hayTesoro c
-- Precondicion: no tiene

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (Tesoro:_) = True
tieneTesoro (_:xs) = tieneTesoro xs
-- Precondición: no tiene

esTesoro::Objeto->Bool
esTesoro Tesoro = True
esTesoro _ = False
-- Precondición: no tiene

  -- 2) Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "No hay tesoro en el camino"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obs c) = if tieneTesoro obs
                                 then 0
                                 else 1 + pasosHastaTesoro c

  -- 3)  Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin             = False
hayTesoroEn n (Nada c)      = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre obj c) = if n == 0
                              then tieneTesoro obj
                              else hayTesoroEn (n-1) c
-- Precondicion: tiene que haber al menos un tesoro.

  -- 4)  Indica si hay al menos n tesoros en el camino
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True                    
alMenosNTesoros _ Fin = False                 
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre obs c) = 
    let encontrados = length (losTesorosEn obs)
    in if encontrados >= n 
       then True
       else alMenosNTesoros (n - encontrados) c

losTesorosEn::[Objeto]->[Objeto]
losTesorosEn [] = []
losTesorosEn (ob:obs) = if esTesoro ob
                        then ob : losTesorosEn obs
                        else losTesorosEn obs

  -- 5) Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre i f c = contarEntre 0 i f c

contarEntre :: Int -> Int -> Int -> Camino -> Int
contarEntre _ _ _ Fin = 0
contarEntre pos i f (Nada c) = contarEntre (pos+1) i f c
contarEntre pos i f (Cofre obs c) = sumarTesorosEnRango pos i f obs + contarEntre (pos+1) i f c

sumarTesorosEnRango::Int->Int->Int->[Objeto]->Int
sumarTesorosEnRango pos i f obs =
  if enRango pos i f 
  then length (losTesorosEn obs)
  else 0

enRango :: Int -> Int -> Int -> Bool
enRango pos i f = pos >= i && pos <= f

{-  2. Tipos arbóreos
 2.1. Árboles binarios
 Dada esta definición para árboles binarios -}
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arb0 = EmptyT
arb1 = NodeT (1::Int) (EmptyT) (EmptyT)
arb2 = NodeT (5::Int) arb1 arb0
arb3 = NodeT (2::Int) arb2 arb1
-- defina las siguientes funciones utilizando recursión estructural según corresponda:

-- 1) Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT::Tree Int->Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2
-- Precondicion: no tiene

  -- 2) Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT::Tree a->Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2
-- Precondicion: no tiene

  -- 3)  Dado un árbol de enteros devuelve un árbol con el doble de cada número
mapDobleT::Tree Int->Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)
-- Precondicion: ninguna

  -- 4) Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT::Eq a=>a->Tree a->Bool
perteneceT _ EmptyT = False
perteneceT x (NodeT y t1 t2) = (x==y) || (perteneceT x t1) || (perteneceT x t2)
-- Precondicion: ninguna

  -- 5) Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e
aparicionesT::Eq a=>a->Tree a->Int
aparicionesT _ EmptyT = 0
aparicionesT x (NodeT y t1 t2) = unoSi (x==y) + (aparicionesT x t1) + (aparicionesT x t2)
-- Precondicion: ninguna

  -- 6)  Dado un árbol devuelve los elementos que se encuentran en sus hojas. NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves::Tree a->[a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x t1 t2) = singularSi x (esHoja t1 t2)  ++ leaves t1 ++ leaves t2
-- Precondicion: ninguna

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = []
-- Precondicion: ninguna

esHoja :: Tree a -> Tree a -> Bool
esHoja EmptyT EmptyT = True
esHoja _      _      = False
-- Precondicion: ninguna

  -- 7)  Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x n1 n2) = 1 +  max (heightT n1) (heightT n2)

  -- 8) Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT            = EmptyT
mirrorT (NodeT x n1 n2 )  =  (NodeT x (mirrorT n2) (mirrorT n1) )    

  -- 9) Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order. Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList  EmptyT         = []
toList (NodeT x n1 n2) = toList n1 ++ [x] ++ toList n2

  -- 10) Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT         = []
levelN 0 (NodeT x n1 n2)= [x]
levelN n (NodeT x n1 n2)= levelN (n-1) n1 ++ levelN (n-1) n2

  -- 11)  Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]]   -> [[a]]
juntarNiveles  []     yss        = yss
juntarNiveles  xss    []         = xss
juntarNiveles  (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

  -- 12) Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x n1 n2) = if heightT n1 > heightT n2
                                  then x : ramaMasLarga n1
                                  else x : ramaMasLarga n2                 

   {-13)  Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
 hasta cualquiera de los nodos.  ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja), o sea, por ejemplo
 todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
 EmptyT)
 (NodeT 4 (NodeT 5 EmptyT EmptyT)
 EmptyT))
 = [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
 OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
 ésta para devolver solamente los caminos maximales.-}
todosLosCaminos :: Tree a         -> [[a]]
todosLosCaminos    EmptyT          = [   ]
todosLosCaminos    (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)
  
consACada :: a -> [[a]]   -> [[a]]
consACada    x    [   ]    = [   ]
consACada    x    (xs:xss) = (x:xs) : consACada x xss 

-----------------------------------------------
{- 2.2. Expresiones Aritméticas
 El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
 data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
 Implementar las siguientes funciones utilizando el esquema de recursión estructural sobre Exp:
-}
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

expresionA1 = Sum (Prod (Valor 2) (Valor 25) ) 
                 (Prod(Valor 1) (Neg  (Neg (Valor 5)) ) )
expresionA2 = Prod ( Sum (Valor 10) (Valor 10) )
                    (Prod  (Valor 5) (Valor 6)  )


  -- 1) Dada una expresión aritmética devuelve el resultado evaluarla. 
eval :: ExpA -> Int  
eval (Valor n)    = n
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e1 )    = eval e1 * (-1)

  -- 2)  Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
simplificar :: ExpA -> ExpA
simplificar (Valor n)    = Valor n
simplificar (Sum e1 e2)  = simplificarSum  (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Neg e1)     = simplificarNeg (simplificar e1)        

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0 ) e         = e
simplificarSum e          (Valor 0) = e
simplificarSum e1         e2        = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0)  _         = Valor 0
simplificarProd  _         (Valor 0) = Valor 0
simplificarProd  e         (Valor 1) = e
simplificarProd (Valor 1)  e         = e
simplificarProd e1        e2        = Prod e1 e2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e) = e
simplificarNeg e       = Neg e