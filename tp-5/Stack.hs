{-4. Stack (pila)
Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:-}

module Stack
(Stack, emptyStk, isEmptyStk, push, top, pop, lenS)
where

data Stack a = Stk [a] Int deriving Show

emptyStk :: Stack a
-- Describe una pila vacía.
-- Costo O(1).
emptyStk = Stk [] 0

isEmptyStk :: Stack a-> Bool
-- Dada una pila indica si está vacía.
-- n = es el tamaño de la lista.
-- (==) -> O(1)
-- Solo se consulta si el tamaño de la lista es 0, por eso es O(1).
isEmptyStk (Stk _ n) = n == 0 -- (o isEmptyStk (Stk xs _) = null xs)

push :: a-> Stack a-> Stack a
-- Dados un elemento y una pila, describe el resultado de agregar el elemento a la pila.
-- N = cantidad de elementos de la lista.
-- n = es el tamaño de la lista.
-- (:) y (+) -> O(1)
-- Se agrega un elemento a la lista y se suma 1 al tamaño, por eso es O(1).
push x (Stk xs n) = Stk (x : xs) (n+1)

top :: Stack a-> a
-- Dada un pila describe el elemento del tope de la pila.
-- N = cantidad de elementos de la lista.
-- head -> O(1)
-- Solo retorna el primer elemento de la lista, por eso es O(1).
top (Stk [] _) = error "La pila está vacía"
top (Stk xs _) = head xs 

pop :: Stack a-> Stack a
-- Dada una pila describe la pila sin el primer elemento.
-- N = cantidad de elementos de la lista.
-- n = es el tamaño de la lista.
-- tail y (-) -> O(1)
-- Descarta el primer elemento y deja el resto intacto, por eso es O(1).
pop (Stk [] _) = error "La pila está vacía"
pop (Stk xs n) = Stk (tail xs) (n-1)

lenS :: Stack a-> Int
-- Dada una pila describe la cantidad de elementos de la misma.
-- n = es el tamaño de la lista.
-- Costo: constante.
lenS (Stk _ n) = n