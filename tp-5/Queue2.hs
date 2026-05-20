-- 2. Implemente ahora la versión que agrega por delante y quita por el nal de la lista. Compare
-- la e ciencia entre ambas implementaciones.

{- Una Queue es un tipo abstracto de datos de naturaleza FIFO ( rst in, rst out). Esto signi ca
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). Su interfaz es la siguiente: -}

module Queue2
    (Queue2, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue2 a = Q [a] deriving Show

emptyQ :: Queue2 a
-- Crea una cola vacía.
-- Q y [] -> O(1)
-- Es solo un constructor con lista vacía, costo O(1).
emptyQ = Q []

isEmptyQ :: Queue2 a-> Bool
-- Dada una cola indica si la cola está vacía.
-- n = cantidad de elementos de la cola.
-- null -> O(1)
-- Solo consulta si la cola no tiene al menos un elemento. Su costo es O(1).
isEmptyQ (Q xs) = null xs

enqueue :: a-> Queue2 a-> Queue2 a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- x = elemento a agregar a la cola.
-- n = cantidad de elementos de la cola.
-- Q y (:) -> O(1)
-- Solo agrega por delante un elemento a la cola y retorna la Queue, su costo es O(1).
enqueue x (Q xs) = Q (x:xs)

firstQ :: Queue2 a-> a
-- Dada una cola describe el primer elemento de la cola.
-- n = cantidad de elementos de la cola.
-- last -> O(n)
-- Se recorre toda la cola para tomar el último elemento, su costo es O(n).
firstQ (Q []) = error "La cola está vacía"
firstQ (Q xs) = last xs

dequeue :: Queue2 a-> Queue2 a
-- Dada una cola la describe sin su primer elemento.
-- n = cantidad de elementos de la cola.
-- reverse -> O(n) / tail -> O(1)
-- Se recorre toda la cola para invertirla y luego se descarta el primer elemento,
-- haciendo que se desencole por detrás, su costo entonces es O(n) + O(1) = O(n).
dequeue (Q []) = error "La cola está vacía"
dequeue (Q xs) = Q (tail (reverse xs)) 

-- ENCOLAR frente -> [...] <- detrás DESENCOLAR