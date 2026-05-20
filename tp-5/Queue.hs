-- 1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse 
-- por el final de la lista y desencolarse por delante.

{-3. Queue (cola)
Una Queue es un tipo abstracto de datos de naturaleza FIFO ( rst in, rst out). Esto signi ca
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). Su interfaz es la siguiente:-}

module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] deriving Show
-- Invariante de representación:
-- - FIFO: el primer elemento que entra a la cola, es el primero en salir.

emptyQ :: Queue a
-- Crea una cola vacía.
-- Q y [] -> O(1)
-- Solo construye una Queue vacía, por eso su costo es O(1).
emptyQ = Q []

isEmptyQ :: Queue a-> Bool
-- Dada una cola indica si la cola está vacía.
-- n = cantidad de elementos de la cola.
-- null -> O(1).
-- Solo consulta si la cola tiene al menos un elemento. Su costo es O(1).
isEmptyQ (Q xs) = null xs

enqueue :: a-> Queue a-> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- x = elemento a encolar por el final de la lista.
-- n = cantidad de elementos de la cola.
-- [] -> O(1) / (++) -> O(n).
-- Agrega los n elementos de la lista a la lista con el elemento a encolar, para que
-- este quede al final. Por eso su costo es O(n).
enqueue x (Q xs) = Q (xs ++ [x])

firstQ :: Queue a-> a
-- Dada una cola describe el primer elemento de la cola.
-- n = cantidad de elementos de la cola.
-- [] y head -> O(1)
-- Solo toma el primer elemento. Su costo es O(1) + O(n) = O(n). 
firstQ (Q []) = error "La cola está vacía"
firstQ (Q xs) = head xs

dequeue :: Queue a-> Queue a
-- Dada una cola la describe sin su primer elemento.
-- n = cantidad de elementos de la cola.
-- [] y tail -> O(1)/
-- Solo descarta el primer elemento para deseconcolarse por delante. Su costo es O(1).
dequeue (Q []) = error "La cola está vacía"
dequeue (Q xs) = Q (tail xs)

-- DESENCOLAR frente -> [...] <- detrás ENCOLAR