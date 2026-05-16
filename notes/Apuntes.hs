-- Recursion Estructural sobre listas.
f :: [a] -> b
f [] = ...  -- Caso Base: qué hacer cuando la lista está vacía.
f (x:xs) = ... x ... f xs ... -- Caso Recursivo: combinar el elemento 'x' con el llamado recursivo sobre la cola 'xs'.

{- Aplicacion: Sirve para cualquier función que recorra una lista elemento por elemento. 
Si se necesita procesar x con complejidad, llamar a una subtarea del lado derecho. -}


-- Recursión Estructural sobre Números.
f :: Int -> b
f 0 = ... -- Caso Base: resultado directo para el 0.
f n = ... n ... f (n-1) ... -- Caso Recursivo: combinar el número 'n' con el llamado recursivo de su anterior.

{- Aplicacion: Esquema para funciones como sumatorias, factoriales, o cuando hay que repetir una 
acción n cantidad de veces.-}

-- Tipos Algebraicos Recursivos Lineales
{- Definición de ejemplo: -} data Pizza = Prepizza | Capa Ingrediente Pizza.
f :: Pizza -> b
f Prepizza = ... -- Caso Base: el constructor que NO hace uso de la recursión.
f (Capa ing p) = ... (auxiliar ing) ... f p ... -- Caso Recursivo: extraer el dato ('ing') y hacer la recursión sobre el resto de la estructura ('p').

{- Aplicacion: Se aplica esto siempre que se vea tipos definidos con data que encadenan un solo camino posible. 
Todo análisis profundos del dato interno (ing) deben delegarse obligatoriamente a una función auxiliar. -}

-- Árboles Binarios (Tree a).
{- Definición de ejemplo: -} data Tree a = EmptyT | NodeT a (Tree a) (Tree a).
f :: Tree a -> b
f EmptyT = ... -- Caso Base: qué hacer al llegar a una rama vacía.
f (NodeT x t1 t2) = ... (auxiliar x) ... f t1 ... f t2 ... -- Caso Recursivo: procesar la raíz 'x', y llamar recursivamente a TODAS sus ramas ('t1' y 't2').

{- Aplicacion: Al tener dos partes recursivas en su constructor, es obligatorio llamar a la función f 
dos veces del lado derecho del igual. -}

-- Funciones que recorren dos listas al mismo tiempo (Tipo Zip)
f :: [a] -> [b] -> c
f [] _ = ...
f (x:xs) [] = ...
f (x:xs) (y:ys) = ... x ... y ... f xs ys ...


{-
Si espera un Número (ej: -> Int)
Caso Base: Tiene que ser un número literal, por ejemplo 0.  

Caso Recursivo: Unir el elemento actual con el resultado de la recursión usando operadores 
matemáticos (+, -, *, max, min).


Si espera una Lista (ej: -> [a])
Caso Base: Tiene que ser una lista, por lo general, la lista vacía [].  

Caso Recursivo: Unir el elemento actual con el resultado de la recursión usando constructores de listas 
(el operador : para pegar un elemento suelto, o el ++ para concatenar dos listas).


Si espera un Booleano (ej: -> Bool)
Caso Base: Tiene que ser True o False.  

Caso Recursivo: Unir la comprobación de el elemento actual con el resultado de la recursión usando operadores lógicos 
(&& para exigir que todo cumpla, o || para buscar si al menos uno cumple).


Si espera una Tupla (ej: -> (Int, Pizza))
Caso Base: Tiene que ser una tupla literal (ej: (0, Prepizza)).

Caso Recursivo: Armar los paréntesis ( , ) y asegurar de que lo que se va a poner del 
lado izquierdo de la coma sea un Int y lo del lado derecho sea una Pizza.
-}

-- ============================

-- Clase5.hs
-- Repaso completo: TADs, Invariantes de representación y Eficiencia

-- ============================
-- Qué es un TAD
-- ============================
-- Un Tipo Abstracto de Datos (TAD) se define por:
-- 1. Su interfaz de operaciones (qué hace).
-- 2. Sus invariantes de representación (reglas internas).
-- 3. Su implementación (cómo lo hace).
--
-- El usuario solo ve la interfaz, no la implementación.

-- ============================
-- Ejemplo: TAD Persona
-- ============================
module Clase5 where

data Persona = P String String Int  -- nombre, apellido, edad

-- Interfaz
nacer :: String -> String -> Persona
nacer nom ape = P nom ape 0

crecer :: Persona -> Persona
crecer (P n a e) = P n a (e+1)

nombre :: Persona -> String
nombre (P n _ _) = n

apellido :: Persona -> String
apellido (P _ a _) = a

edad :: Persona -> Int
edad (P _ _ e) = e

-- Invariante de representación:
-- edad >= 0, nombre y apellido no vacíos.
-- Esto asegura que los datos siempre sean válidos.

-- ============================
-- Ejemplo: TAD Termómetro
-- ============================
data Termometro = T [Int] [Int]  -- lista de mediciones + máximos guardados desde cada posición

nuevoT :: Termometro
{- Solo construye la instancia de Termometro con sus listas de temperaturas vacías.
    Costo O(1) -}
nuevoT = T [] []

ingresarT :: Int -> Termometro -> Termometro
{- Solo construye un nuevo termometro con la nueva temperatura dada. El agregado es costo constante.
    Costo O(1) -}
ingresarT t (T ts ms) = T (t:ts) agregarSiEsMaximo t ms

agregarSiEsMaximo :: Int -> [Int] -> [Int] -- Auxiliar
{- Solo hace operaciones lineales para un solo elemento.
    Costo O(1) -}
agregarSiEsMaximo _ [] = []
agregarSiEsMaximo t ms = let maxActual = head ms -- O(1)
    in if t > maxActual then t : ms else maxActual : ms -- (:) y (>) -> O(1)

sinTemps :: Termometro -> Bool
{- Evalúa si la lista tiene elementos, y eso es una operación constante.
    Costo O(1) -}
sinTemps (T ts _) = null ts -- O(1)

ultimaT :: Termometro -> Int
{- Retorna la primera ocurrencia, y eso es una operación constante.
    Costo O(1) -}
ultimaT (T [] _) = error "No existen temperaturas registradas"
ultimaT (T ts _) = head ts -- O(1)

quitarUltimaT :: Termometro -> Termometro
{- Hace uso de la misma operación constante para las dos listas.
    Costo O(1). -}
quitarUltimaT (T [] _) = error "No existen temperaturas registradas"
quitarUltimaT (T ts ms) = T (tail ts) (tail ms) -- O(1)

maxT :: Termometro -> Int
{- Retorna la primera ocurrencia de la lista de máximos.
    Costo O(1). -}
maxT (T [] _) = error "No existen temperaturas registradas"
maxT (T _ ms) = head ms -- O(1)

-- Invariante de representación:
-- ts y ms tienen la misma longitud
-- Cada m de ms es el máximo de la sublista de ts desde esa pos.
-- Esto garantiza eficiencia en maxT (O(1)).

-- ============================
-- Eficiencia
-- ============================
-- Se mide en peor caso (worst case).
-- Ejemplos:
-- O(1): operaciones constantes → acceder al primer elemento.
    -- ❏ La función tarda siempre lo mismo
    -- ❏ No depende de la cantidad de elementos de la estructura
    -- ❏ Ejemplos:
    --     ❏ head
    --     ❏ tail
    --     ❏ null

-- O(n): operaciones lineales → recorrer una lista.
    -- ❏Por cada elemento de la estructura, solamente se hacen 
    -- operaciones de costo constante ( O(1) * n )
    -- ❏ Ejemplos:
    --     ❏ length
    --     ❏ sum
    --     ❏ elem

-- O(n^2): operaciones cuadráticas → comparar todos con todos.
    -- ❏Por cada elemento de la estructura, se hacen operaciones 
    -- de costo constante ( O(n) * n )
    -- ❏Otro Ejemplo
        -- unoConTodos :: a -> [a] -> [(a,a)]  -- lineal
        -- unoConTodos x []     = []
        -- unoConTodos x (y:ys) = (x,y) : unoConTodos x ys
        --                     -- constante


-- ============================
-- Cuadro comparativo
-- ============================

{- 
Stack (Pila)   | Queue (Cola)   | Set (Conjunto)
-----------------------------------------------
Modelo: LIFO   | Modelo: FIFO   | No admite repetidos
push: O(1)     | enqueue: O(1)  | insert: O(1) promedio
pop: O(1)      | dequeue: O(1)  | pertenece: O(1) promedio
peek: O(1)     | front: O(1)    | delete: O(1) promedio
Uso: ejecución | Uso: colas de  | Uso: mantener elementos
de programas   | procesos, turnos| únicos, sin duplicados
-}

-- ============================
-- Idea central
-- ============================
-- Definir un TAD = interfaz + invariantes + implementación.
-- Invariantes = reglas internas que aseguran validez y eficiencia.
-- Eficiencia = medir costo en peor caso (O(1), O(n), O(n^2)).
