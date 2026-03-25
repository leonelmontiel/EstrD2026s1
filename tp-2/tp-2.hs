{-De na las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario:-}
----------

sumatoria :: [Int]-> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos.
--Precondición: ninguna
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns
----------

longitud :: [a]-> Int
{-Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee.
Precondición: ninguna-}
longitud [] = 0
longitud (_:es) = 1 + longitud es
----------

sucesores :: [Int]-> [Int]
{- Dada una lista de enteros, devuelve la lista de los sucesores
de cada entero.
Precondición: ninguna-}
sucesores [] = []
sucesores (n:ns) = (n+1) : sucesores ns
----------

conjuncion :: [Bool]-> Bool
{-Dada una lista de booleanos devuelve True si todos sus elementos son True.
Precondicion: ninguna-}
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs
----------

disyuncion :: [Bool]-> Bool
{-Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
Precondicion: ninguna-}
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs
----------

aplanar :: [[a]]-> [a]
{-Dada una lista de listas, devuelve una única lista con todos sus elementos.
Precondicion: ninguna-}
aplanar [] = []
aplanar (ls:lss) = ls ++ aplanar lss
----------

pertenece :: Eq a => a-> [a]-> Bool
{-Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.
Precondición: ninguna-}
pertenece _ [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs
----------

apariciones :: Eq a => a-> [a]-> Int
{-Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
Precondición: -}
apariciones _ [] = 0
apariciones e (x:xs) = unoSi e x + apariciones e xs

unoSi::Eq a => a -> a -> Int
{- Dados un elemento x y un elemento y, retorna 1 si coinciden, 0 sino.
Precondicion: ninguna -}
unoSi x y = if x==y then 1 else 0
----------

losMenoresA :: Int-> [Int]-> [Int]
{-Dados un número n y una lista xs, devuelve todos los
elementos de xs que son menores a n.
Precondición: ninguna-}
losMenoresA _ [] = []
losMenoresA n (x:xs) = if x<n then x:losMenoresA n xs else losMenoresA n xs
{-losMenoresA n (x:xs) = guardarSiEsMenorA x n ++ losMenoresA n xs

guardarSiEsMenorA :: Int -> Int -> [Int]
guardarSiEsMenorA n m = if n<m then [n] else []-}
----------

lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
{-Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos.
Precondición: ninguna-}
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (ls:lss) = 
    if longitud ls > n 
    then ls:lasDeLongitudMayorA n lss 
    else lasDeLongitudMayorA n lss
----------

agregarAlFinal :: [a]-> a-> [a]
{-Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la
lista.
Precondición: ninguna-}
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e
----------

agregar :: [a]-> [a]-> [a]
{-Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. De nida en Haskell como (++).
Precondición: ninguna-}
agregar xs [] = xs
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys
----------

reversa :: [a]-> [a]
{-Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. De nida
en Haskell como reverse.
Precondición: ninguna-}
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
----------

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
----------

elMinimo :: Ord a => [a]-> a
{-Dada una lista devuelve el mínimo
Precondición: la lista no puede estar vacía-}
elMinimo [] = error "La lista no puede estar vacía"
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs then x else elMinimo xs
----------

{-2. Recursión sobre números
De na las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
lo contrario:-}
----------

factorial :: Int-> Int
{-Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
Precondición: n no puede ser negativo-}
factorial 0 = 1
factorial n = 
    if n<0
    then error "n no puede ser negativo"
    else n * factorial (n-1)
----------

cuentaRegresiva :: Int-> [Int]
{-Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
Precondición: ninguna-}
cuentaRegresiva n = 
    if n<1
    then []
    else n : cuentaRegresiva (n-1)
----------

repetir :: Int-> a-> [a]
{-Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
Precondición: n no puede ser negativo-}
repetir 0 e = []
repetir n e =
    if n<0
    then error "n no puede ser negativo"
    else e : repetir (n-1) e
----------

losPrimeros :: Int-> [a]-> [a]
{-Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.
Precondición: n no puede ser negativo-}
losPrimeros 0 xs = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = 
    if n<0
    then error "n no puede ser negativo"
    else x : losPrimeros (n-1) xs
----------

sinLosPrimeros :: Int-> [a]-> [a]
{-Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa.
Precondición: n no puede ser negativo-}
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (_:xs) =
    if n<0
    then error "n no puede ser negativo"
    else sinLosPrimeros (n-1) xs
----------

{-3. Registros
1. De nir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}
data Persona = P String Int deriving Show
--               nombre edad
p1 = P "Leo" 30
p2 = P "Jorge" 63
p3 = P "Mariana" 50
----------

mayoresA :: Int-> [Persona]-> [Persona]
{-Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
Precondición: n no puede ser negativo-}
mayoresA _ [] = []
mayoresA n (p:ps) = 
    if edad p > (verificaSigno n)
    then p : mayoresA n ps
    else mayoresA n ps

edad::Persona->Int
{-Dada una persona retorna su edad.
Precondición: ninguna-}
edad (P _ e) = e

verificaSigno::Int->Int
{- Dado un número lo retorna si es positivo, sino lanza error.
Precondición: n no puede ser negativo-}
verificaSigno n =
    if n<0
    then error "n no puede ser negativo"
    else n
----------

promedioEdad :: [Persona]-> Int
{-Dada una lista de personas devuelve el promedio de edad entre esas personas. Precon
dición: la lista al menos posee una persona.
Precondición: la lista de personas no puede estar vacía-}
promedioEdad [] = error "La lista de personas no puede estar vacía"
promedioEdad ps = div (sumatoria (edades ps)) (longitud ps)

edades::[Persona]->[Int]
{-Dada una lista de personas retorna la lista con sus respectivas edades
Precondición: ninguna-}
edades [] = []
edades (p:ps) = edad p : edades ps
----------

elMasViejo :: [Persona]-> Persona
{-Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona.
Precondición: la lista al menos posee una persona-}
elMasViejo [] = error "la lista al menos posee una persona"
elMasViejo [p] = p
elMasViejo (p:ps) = 
    if edad p > edad(elMasViejo ps)
    then p
    else elMasViejo ps
----------

{-2. Modi caremos la representación de Entreador y Pokemon de la práctica anterior de la si
guiente manera:-}
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pk1 = ConsPokemon Agua 80
pk2 = ConsPokemon Fuego 55
pk3 = ConsPokemon Planta 100
e1 = ConsEntrenador "Ash" [pk2, pk2, pk3, pk1]
e2 = ConsEntrenador "Brook" [pk1 ,pk2, pk3]
e3 = ConsEntrenador "Misty" [pk3]
e4 = ConsEntrenador "Jessie" []
e5 = ConsEntrenador "James" [pk1, pk2]
{-Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
De nir en base a esa representación las siguientes funciones:-}
----------

cantPokemon :: Entrenador-> Int
{-Devuelve la cantidad de Pokémon que posee el entrenador.
Precondición: ninguna-}
cantPokemon (ConsEntrenador _ pks) = longitud pks
----------

cantPokemonDe :: TipoDePokemon-> Entrenador-> Int
{-Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
Precondición: ninguna-}
cantPokemonDe t ent = cantPokemonesDeEn t (pokemonesDe ent)

pokemonesDe :: Entrenador -> [Pokemon]
{-Dado un entrenador retorna la lista de Pokemones que le pertenecen.
Precondición: ninguna-}
pokemonesDe (ConsEntrenador _ pks) = pks

cantPokemonesDeEn :: TipoDePokemon -> [Pokemon] -> Int
{-Dado un tipo de pokemón y una lista de pokemones, retorna la cantidad 
de pokemones de dicho tipo que le pertenecen.
Precondición: ninguna-}
cantPokemonesDeEn _ [] = 0
cantPokemonesDeEn t (pk:pks) = unoSiCoincideTipo t pk + cantPokemonesDeEn t pks

unoSiCoincideTipo::TipoDePokemon->Pokemon->Int
--Retorna 1 si el tipo dado coincide con el tipo del pokemon dado.
--Precondición: no tiene
unoSiCoincideTipo t1 (ConsPokemon t2 _) =
    if esDeIgualTipo t1 t2
        then 1
        else 0

esDeIgualTipo::TipoDePokemon->TipoDePokemon->Bool
--Indica si el primer tipo de pokemon es del mismo tipo que del segundo.
--Precondición: no tiene
esDeIgualTipo Agua Agua = True
esDeIgualTipo Fuego Fuego = True
esDeIgualTipo Planta Planta = True
esDeIgualTipo _ _ = False
----------