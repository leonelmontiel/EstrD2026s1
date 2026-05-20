{-3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la e ciencia obtenida en esta implementación con la anterior.-}
module Set2
    (Set2, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set2 a = S [a] deriving Show
-- Invariantes de representación:
-- - La lista interna PUEDE contener elementos repetidos.

emptyS :: Set2 a
-- Crea un conjunto vacío.
-- [] -> O(1)
-- Es solo un constructor con lista vacía, O(1).
emptyS = S []

addS :: Eq a => a-> Set2 a-> Set2 a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- x = es el elemento a agregar
-- n = cantidad de elementos de la lista
-- (:) -> O(1)
-- se agrega el elemento x a la lista, por eso es O(1).
addS x (S xs) = S (x:xs)

belongs :: Eq a => a-> Set2 a-> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- x = es el elemento a buscar en la lista
-- n = cantidad de elementos de la lista
-- elem -> O(n^2)
-- en el peor de los casos, se recorre toda la lista para buscar y no encontrar el elemento
-- buscado. Por eso su costo es de O(n^2).
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set2 a-> Int
-- Describe la cantidad de elementos distintos de un conjunto.
-- n = cantidad de elementos de la lista.
-- sinRepetidos -> O(n) / length -> O(n)
-- Se aplica primero el sinRepetidos y luego el length, quedando su costo en
-- O(n) + O(n) = O(n).
sizeS (S xs) = length (sinRepetidos xs)

-- Auxiliar de sizeS
-- n = cantidad de elementos de la lista
-- elem -> O(n) / (:) -> O(1)
-- En el peor de los casos, x no está en la lista, por eso se la recorre hasta el final,
-- consultando si ese elemento está en el resto de la lista. Además, aplica recursión
-- sobre el resto de los elementos. Su costo entonces es O(n) * O(n) = O(n^2).
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

removeS :: Eq a => a-> Set2 a-> Set2 a
-- Borra un elemento del conjunto.
-- n = cantidad de elementos de la lista
-- m = cantidad de elementos del conjunto
-- removeAllOcurrences -> O(n) / S -> O(1)
-- Se delega la operación principal a removeAllOcurrencesOf, por eso su costo es O(n).
removeS x (S xs) = S (removeAllOcurrencesOf x xs)

-- Auxiliar de removeS
removeAllOcurrencesOf :: Eq a => a -> [a] -> [a]
-- x = elemento a remover de la lista
-- n = cantidad de elementos de la lista
-- (==) y (:) -> O(1)
-- No importa el caso, siempre recorrerá toda la lista para encontrar y eliminar la ocurrencia
-- repetida de x, haciendo uso de la recursión. Su costo es O(n).
removeAllOcurrencesOf _ [] = []
removeAllOcurrencesOf x (y:ys) = 
    if x==y 
        then removeAllOcurrencesOf x ys 
        else y : removeAllOcurrencesOf x ys

unionS :: Eq a => Set2 a-> Set2 a-> Set2 a
-- Dados dos conjuntos describe un conjunto con todos los elementos de ambos. conjuntos.
-- n = cantidad de elementos del conjunto xs
-- m = cantidad de elementos del conjunto ys
-- (++) -> O(n) / S -> O(1)
-- Se agregan todos los elementos del conjunto xs al conjunto ys, por eso su costo tiene en
-- en cuenta los n elementos, siendo así O(n).
unionS (S xs) (S ys) = S (xs++ys)

setToList :: Eq a => Set2 a-> [a]
-- Dado un conjunto describe una lista con todos los elementos distintos del conjunto.
-- n = cantidad de elementos del conjunto
-- sinRepetidos -> O(n) 
-- Recorre toda la lista para quitar los elementos que se repiten, siendo así su costo O(n).
setToList (S xs) = sinRepetidos xs