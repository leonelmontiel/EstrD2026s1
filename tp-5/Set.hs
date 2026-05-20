{- 1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
la cantidad de elementos en la estructura.
Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos. -}

-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:
module Set 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] deriving Show
-- Invariantes de representación:
-- - El conjunto no contiene elementos repetidos.

emptyS :: Set a
-- Crea un conjunto vacío.
-- S -> O(1)
-- Es un constructor, por ende su costo es O(1).
emptyS = S []

addS :: Eq a => a-> Set a-> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- x = elemento a agregar al set.
-- n = cantidad de elementos del conjunto.
-- elem -> O(n) / (:) -> O(1) / S -> O(1)
-- En el peor de los casos recorre toda la lista, no encuentra el elemento buscado y
-- lo agrega usando una operación constante. Su costo total es O(n).
addS x (S xs) =
    if elem x xs
        then S xs
        else S (x : xs)

belongs :: Eq a => a-> Set a-> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- x = elemento a buscar en el conjunto.
-- n = cantidad de elementos del conjunto.
-- elem -> O(n)
-- como se hace una única llamada a elem, el costo de belongs también es O(n).
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a-> Int
-- Describe la cantidad de elementos distintos de un conjunto.
-- n = cantidad de elementos del conjunto.
-- length -> O(n)
-- Como la invariante garantiza que no hay elementos repetidos en el conjunto, se recorre
-- todo el conjunto para retornar n. Su costo es O(n).
sizeS (S xs) = length xs

removeS :: Eq a => a-> Set a-> Set a
-- Borra un elemento del conjunto.
-- x = elemento a buscar en el conjunto.
-- n = cantidad de elementos del conjunto.
-- S -> O(1) / removeElemInSet -> O(n)
-- como solo se delega la lógica a una subtarea con costo lineal, removeS también es O(n).
removeS x (S xs) = S (removeElemInSet x xs)

removeElemInSet :: Eq a => a -> [a] -> [a]
-- x = elemento a buscar en la lista.
-- n = cantidad de elementos de la lista.
-- (==) y (:) -> O(1)
-- en el peor de los casos, se recorre toda la lista sin encontrar el elemento buscado,
-- por ende su costo es lineal O(n).
removeElemInSet _ [] = []
removeElemInSet x (y:ys) =
    if x == y
        then ys
        else y : removeElemInSet x ys

unionS :: Eq a => Set a-> Set a-> Set a
-- Dados dos conjuntos describe un conjunto con todos los elementos de ambos conjuntos.
-- n = cantidad de elementos de la lista.
-- m = cantidad de elementos del conjunto.
-- S -> O(1) / mergearSets -> O(n * m + n^2)
-- se delega la operación principal a mergearSets, por ende unionS es O(n * m + n^2).
unionS (S xs) set = mergearElems xs set

-- Auxiliar de unionS
mergearElems :: Eq a => [a] -> Set a -> Set a
-- n = cantidad de elementos de la lista.
-- m = cantidad de elementos del conjunto.
-- addS -> O(m)
-- En cada paso ejecuta addS, cuyo costo es lineal respecto al tamaño del
-- conjunto acumulado (que empieza en m y llega hasta m+n).
-- Costo Total: O(n * (m + n)) = O(n * m + n^2)
mergearElems [] set = set
mergearElems (x:xs) set = mergearElems xs (addS x set)

setToList :: Eq a => Set a-> [a]
-- Dado un conjunto describe una lista con todos los elementos distintos del conjunto.
-- n = cantidad de elementos del conjunto
-- S -> O(1)
-- solo retorna la lista que contiene el Set. Cabe recalcar que se respeta la invariante
-- al decir que son elementos distintos. Su costo total es O(1).
setToList (S xs) = xs