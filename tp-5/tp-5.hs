import Set;

{-1. Cálculo de costos
Especi car el costo operacional de las siguientes funciones:--}
-- Solo devuelve el primer elemento de la lista, por eso su costo es constante O(1).
{- la función solo accede directamente al primer elemento mediante el emparejamiento 
de patrones (pattern matching) y no necesita recorrer el resto de la estructura (xs),
 sin importar si la lista tiene diez o diez millones de elementos. -}
-- falta caso base para lista vacía.
head' :: [a]-> a
head' (x:xs) = x

sumar :: Int-> Int
-- suma 9 veces el 1 al elemento dado por parámetro, toma exactamente el mismo tiempo,
-- sin importar si el valor de x es cero, un millón o un número negativo. Esto es una operación constante O(1).
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int-> Int
-- N = la cantidad de reiteraciones de la llamada recursiva
-- se realiza una operación constante por cada una de las llamadas decrecientes de n,
-- es decir, se hace recursión; por eso tiene un costo lineal O(N). Pero es importante recalcar
-- que no tiene un caso base, por ende entra en un loop infinito.
factorial n = n * factorial (n-1)

longitud :: [a]-> Int
-- n = cantidad de elementos de la lista
-- se realiza llamadas recursiva por cada elemento de la lista, para aplicarle la operación constante
-- de la suma, por eso el costo es O(n)
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int]-> [Int]
-- n = cantidad de elementos de la lista
-- k = es el valor de los números dentro de la lista
-- se hace una llamada recursiva por cada uno de los elementos de la lista, para aplicarle una operación
-- lineal, por ese motivo, esta función es cuadrática O(n*k)
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

pertenece :: Eq a => a-> [a]-> Bool
-- n = número a buscar
-- N = cantidad de elementos de la lista
-- (=) -> O(1) / (||) -> O(1)
-- En el peor de los casos (si el elemento no está o está al final), se realiza una llamada 
-- recursiva por cada uno de los elementos de la lista. Por eso el costo de tiempo es O(N).
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

sinRepetidos :: Eq a => [a]-> [a]
-- n = cantidad de elementos de la lista
-- Para cada uno de los n elementos de la lista se realiza una llamada recursiva principal, 
-- pero dentro de cada paso se ejecuta la función 'pertenece', la cual tiene un costo lineal O(n). 
-- Al combinar una operación lineal dentro de un bucle de n pasos, el costo total es cuadrático O(n^2).
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs
        then sinRepetidos xs
        else x : sinRepetidos xs

-- equivalente a (++)
append :: [a]-> [a]-> [a]
-- n = cantidad de elementos de la primera lista
-- m = cantidad de elementos de la segunda lista
-- (:) -> O(1)
-- Se realiza una llamada recursiva por cada uno de los n elementos de la primera lista para
-- reconstruirla al frente de la segunda lista usando la operación constante (:). 
-- Dado que la segunda lista (m) no se recorre, el costo total es lineal O(n).
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String]-> String
-- n = cantidad de strings en la lista
-- k = longitud promedio de cada string
-- (++) -> O(k) (Costo lineal respecto al tamaño del string x)
-- Se realiza una llamada recursiva por cada uno de los n strings. En cada paso se aplica 
-- el operador (++), el cual debe recorrer los k caracteres del string actual. 
-- Por lo tanto, el costo total es O(n * k).
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int-> [a]-> [a]
-- n = es la cantidad de elementos a tomar de la lista
-- N = cantidad de elementos de la lista
-- (:) -> O(1)
-- en el peor de los casos, (n >= N), entonces se hace recursión por cada uno de los elementos de la lista.
-- Quedando así el costo de la función en O(N).
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int-> [a]-> [a]
-- n = cantidad de elementos a descartar de la lista
-- N = cantidad de elementos de la lista
-- en el peor de los casos, (n >= N), entonces se hace recursión implícitamente por cada uno de los
-- elementos de la lista. Quedando así el costo de la función en O(N).
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int-> [a]-> ([a], [a])
-- n = posición desde donde hacer la división de la lista
-- N = cantidad de elementos de la lista.
-- takeN -> O(N) / dropN -> O(N)
-- Tupla (,) -> O(1)
-- En el peor de los casos (n >= N), se ejecutan consecutivamente ambas funciones lineales 
-- sobre la lista. Esto resulta en O(N) + O(N) = O(2N), lo que simplificado mantiene 
-- un costo de tiempo lineal O(N).
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a]-> a
-- n = cantidad de elementos de la lista.
-- min -> O(1) (retorna el mínimo entre dos elementos)
-- El algoritmo realiza una llamada recursiva por cada uno de los n elementos de la lista. 
-- En cada nivel de la recursión se aplica la función constante min, por lo que el costo total 
-- es la multiplicación de n pasos por una operación constante: n * O(1) = O(n).
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a-> [a]-> [a]
-- n = elemento a quitar de la lista.
-- N = cantidad de elementos de la lista.
-- (==) / (:) -> O(1)
-- en el peor de los casos, el elemento n no está en la lista o está al final.
-- Entonces se hace recursion por cada uno de los N elementos en ella. Quedando el costo en O(N).
sacar n [] = []
sacar n (x:xs) =
    if n == x
        then xs
        else x : sacar n xs

ordenar :: Ord a => [a]-> [a]
-- n = cantidad de elementos de la lista.
-- minimo -> O(n) / sacar -> O(n) / (:) -> O(1)
-- En cada paso de la recursión se realizan de forma secuencial dos operaciones lineales 
-- (minimo y sacar) para obtener y remover el elemento más pequeño, sumando O(n) + O(n) = O(n). 
-- Como este proceso de costo lineal se repite de forma descendente para los n elementos de la 
-- lista, el costo acumulado se comporta como una serie aritmética, resultando en un costo cuadrático O(n^2).
ordenar [] = []
ordenar xs = let m = minimo xs
             in m : ordenar (sacar m xs)

-----------------------------------

-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
losQuePertenecen :: Eq a => [a]-> Set a-> [a]
-- Dados una lista y un conjunto, describe una lista con todos los elementos que pertenecen
-- al conjunto.
-- n = cantidad de elementos de la lista
-- m = cantidad de elementos del conjunto
-- belongs -> O(m) / (:) -> O(1)
-- Por cada n elemento se evalúa si está contenido en el conjunto. Por eso su costo es O(n*m)
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) set =
    if belongs x set
        then x : losQuePertenecen xs set
        else losQuePertenecen xs set

sinRepetidos :: Eq a => [a]-> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto
-- como estructura auxiliar.
-- n = cantidad de elementos de la lista.
-- m = cantidad de elementos del conjunto
-- emptyS -> O(1) / mergearElems -> O(n * m + n^2) / setToList -> O(1)
-- la operación principal es la delegación a mergearElems, la cual tiene un costo O(n * m + n^2).
sinRepetidos xs = setToList (mergearElems xs emptyS)

unirTodos :: Eq a => Tree (Set a)-> Set a
-- Dado un arbol de conjuntos describe un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyT = emptyS
unirTodos (NodeT set tsi tsd) = unionS set (unionS (unirTodos sti) (unirTodos tsd))