{-De na las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario:-}
sumatoria :: [Int]-> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos.
--Precondición: ninguna
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a]-> Int
{-Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee.
Precondición: ninguna-}
longitud [] = 0
longitud (_:es) = 1 + longitud es

sucesores :: [Int]-> [Int]
{- Dada una lista de enteros, devuelve la lista de los sucesores
de cada entero.
Precondición: ninguna-}
sucesores [] = []
sucesores (n:ns) = (n+1) : sucesores ns

conjuncion :: [Bool]-> Bool
{-Dada una lista de booleanos devuelve True si todos sus elementos son True.
Precondicion: ninguna-}
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool]-> Bool
{-Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
Precondicion: ninguna-}
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]]-> [a]
{-Dada una lista de listas, devuelve una única lista con todos sus elementos.
Precondicion: ninguna-}
aplanar [] = []
aplanar (ls:lss) = ls ++ aplanar lss

pertenece :: Eq a => a-> [a]-> Bool
{-Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.
Precondición: ninguna-}
pertenece _ [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

apariciones :: Eq a => a-> [a]-> Int
{-Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
Precondición: -}
apariciones _ [] = 0
apariciones e (x:xs) = unoSi e x + apariciones e xs

unoSi::Eq a => a -> a -> Int
{- Dados un elemento x y un elemento y, retorna 1 si coinciden, 0 sino.
Precondicion: ninguna -}
unoSi x y = if x==y then 1 else 0

losMenoresA :: Int-> [Int]-> [Int]
{-Dados un número n y una lista xs, devuelve todos los
elementos de xs que son menores a n.
Precondición: ninguna-}
losMenoresA _ [] = []
losMenoresA n (x:xs) = if x<n then x:losMenoresA n xs else losMenoresA n xs
{-losMenoresA n (x:xs) = guardarSiEsMenorA x n ++ losMenoresA n xs

guardarSiEsMenorA :: Int -> Int -> [Int]
guardarSiEsMenorA n m = if n<m then [n] else []-}

lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
{-Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos.
Precondición: ninguna-}
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (ls:lss) = 
    if longitud ls > n 
    then ls:lasDeLongitudMayorA n lss 
    else lasDeLongitudMayorA n lss

agregarAlFinal :: [a]-> a-> [a]
{-Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la
lista.
Precondición: ninguna-}
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

agregar :: [a]-> [a]-> [a]
{-Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. De nida en Haskell como (++).
Precondición: ninguna-}
agregar xs [] = xs
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

reversa :: [a]-> [a]
{-Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. De nida
en Haskell como reverse.
Precondición: ninguna-}
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int]-> [Int]-> [Int]
{-Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
Precondición: ninguna-}
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = 
    if x>y
    then x : zipMaximos xs ys
    else y : zipMaximos xs ys

elMinimo :: Ord a => [a]-> a
{-Dada una lista devuelve el mínimo
Precondición: la lista no puede estar vacía-}
elMinimo [] = error "La lista no puede estar vacía"
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs then x else elMinimo xs