{-2. Números enteros
1. De na las siguientes funciones:-}
sucesor::Int->Int
-- Dado un número devuelve su sucesor
--Precondición: no tiene
sucesor n = n + 1

sumar::Int->Int->Int
-- Dados dos números devuelve su suma utilizando la operación +.
--Precondición: no tiene
sumar n m = n + m

divisionYResto::Int->Int->(Int, Int)
{-Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int-> Int-> Int,
provista por Haskell.
--Precondición: el segundo número no puede ser 0.-}
divisionYResto _ 0 = error "No se puede dividir por cero."
divisionYResto n m = (div n m, mod n m)

maxDelPar::(Int, Int)->Int
-- Dado un par de números devuelve el mayor de estos
--Precondición: no tiene
maxDelPar (n,m) = if n > m 
                    then n 
                    else m

{- 2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expre
sión a todas las funciones del punto anterior.
Ejemplo: maxDelPar (divisionYResto (sumar 5 5) (sucesor 0)) -}
--maxDelPar (divisionYResto (sucesor 9) (sumar (-8) 9))
--sucesor (maxDelPar (divisionYResto (sumar 10 8) 2))
--sumar (sucesor 4) (maxDelPar (divisionYResto 15 3))
--sumar (sucesor (sumar (-10) (9))) (maxDelPar (divisionYResto 100 10))

{- 3. Tipos enumerativos
1. De nir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
las siguientes funciones: -}
data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto::Dir->Dir
-- Dada una dirección devuelve su opuesta.
--Precondición: no tiene
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir-> Dir-> Bool
-- Dadas dos direcciones, indica si son la misma.
-- Nota: utilizar pattern matching y no ==.
--Precondición: no tiene
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir-> Dir
{- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste.
¿Posee una precondición esta función? Sí, que no existe una dirección siguiente a Oeste. 
¿Es una función total o parcial? ¿Por qué? Al tener una precondición esta función es parcial. -}
--Precondición: Oeste no tiene siguiente dirección.
siguiente d =
    case d of
        Norte -> Este
        Este -> Sur
        Sur -> Oeste
        Oeste -> error "Oeste no posee una dirección siguiente"

{- 2. De nir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones: -}
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
{- Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar de nir subtareas útiles
que puedan servir después.-}
--Precondición: no tiene
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia::DiaDeSemana
-- retorne el primer día de la semana
--Precondición: no tiene
primerDia = Lunes

ultimoDia::DiaDeSemana
-- retorne el último día de la semana
--Precondición: no tiene
ultimoDia = Domingo

empiezaConM :: DiaDeSemana-> Bool
-- Dado un día de la semana indica si comienza con la letra M.
--Precondición: no tiene
empiezaConM d =
    case d of
        Martes -> True
        Miercoles -> True
        _ -> False

vieneDespues :: DiaDeSemana-> DiaDeSemana-> Bool
{- Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
Ejemplo: vieneDespues Jueves Lunes = True -}
--Precondición: no tiene
vieneDespues dia1 dia2 = valorDeDia dia1 > valorDeDia dia2

valorDeDia::DiaDeSemana->Int
-- retorna el equivalente numérico al orden en el calendario del día de la semana dado
--Precondición: no tiene
valorDeDia d =
    case d of
        Lunes -> 1
        Martes -> 2
        Miercoles -> 3
        Jueves -> 4
        Viernes -> 5
        Sabado -> 6
        Domingo -> 7

estaEnElMedio :: DiaDeSemana-> Bool
-- Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
--Precondición: no tiene
estaEnElMedio d =
    case d of
        Lunes -> False
        Domingo -> False
        _ -> True

{- 3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. De na
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya de nidas en Haskell): -}
negar :: Bool-> Bool
{- Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está de nida como not.-}
--Precondición: no tiene
negar True = False
negar False = True

implica :: Bool-> Bool-> Bool
{- Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.-}
--Precondición: no tiene
implica True y = y
implica False _ = True

yTambien :: Bool-> Bool-> Bool
{- Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está de nida como \&\&.-}
--Precondición: no tiene
yTambien True y = y
yTambien _ _ = False

oBien :: Bool-> Bool-> Bool
{- Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está de nida como ||. -}
--Precondición: no tiene
oBien False y = y
oBien True _ = True

{-4. Registros
1. De nir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}
data Persona = P String Int deriving Show
              -- nombre edad
              -- la edad no debe ser negativa
p1 = P "Leo" 30
p2 = P "Elias" 18

nombre :: Persona-> String
--Devuelve el nombre de una persona
--Precondición: no tiene
nombre (P n _) = n

edad :: Persona-> Int
--Devuelve la edad de una persona
--Precondición: no tiene
edad (P _ e) = e

crecer :: Persona-> Persona
--Aumenta en uno la edad de la persona.
--Precondición: no tiene
crecer p = P (nombre p) (edad p + 1)

cambioDeNombre :: String-> Persona-> Persona
{-Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.-}
--Precondición: no tiene
cambioDeNombre n (P _ e) = P n e

esMayorQueLaOtra :: Persona-> Persona-> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.
--Precondición: no tiene
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona-> Persona-> Persona
--Dadas dos personas devuelve a la persona que sea mayor.
--Precondición: no tiene
laQueEsMayor p1 p2 =
    if edad p1 > edad p2
        then p1
        else p2

{- 2. De nir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego de nir las
siguientes funciones: -}
data Pokemon = PK TipoDePokemon Int deriving Show
               -- tipo          porcentaje
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Entrenador = E String Pokemon Pokemon deriving Show
                 -- nombre pokemon1 pokemon2

pk1 = PK Agua 80
pk2 = PK Fuego 55
pk3 = PK Planta 100
e1 = E "Ash" pk1 pk2
e2 = E "Brook" pk2 pk3
e3 = E "Misty" pk1 pk1

superaA :: Pokemon-> Pokemon-> Bool
{-Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.-}
--Precondición: no tiene
superaA (PK t1 _) (PK t2 _) = primerTipoSuperaAlSegundo t1 t2

primerTipoSuperaAlSegundo::TipoDePokemon->TipoDePokemon->Bool
--Indica si el primer tipo de pokemon supera al segundo tipo.
--Precondición: no tiene
primerTipoSuperaAlSegundo Agua Fuego = True
primerTipoSuperaAlSegundo Fuego Planta = True
primerTipoSuperaAlSegundo Planta Agua = True
primerTipoSuperaAlSegundo _ _ = False

cantidadDePokemonDe :: TipoDePokemon-> Entrenador-> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
--Precondición: no tiene
cantidadDePokemonDe t (E _ pk1 pk2) = unoSiCoincideTipo t pk1 + unoSiCoincideTipo t pk2

unoSiCoincideTipo::TipoDePokemon->Pokemon->Int
--Retorna 1 si el tipo dado coincide con el tipo del pokemon dado.
--Precondición: no tiene
unoSiCoincideTipo t1 (PK t2 _) =
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

juntarPokemon :: (Entrenador, Entrenador)-> [Pokemon]
--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
--Precondición: no tiene
juntarPokemon (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe::Entrenador->[Pokemon]
--Retorna una lista con los pokemones que posee el entrenador dado.
--Precondición: no tiene
pokemonesDe (E _ pk1 pk2) = [pk1, pk2]

{- 5. Funciones polimór cas
1. De na las siguientes funciones polimór cas: -}
loMismo :: a-> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
--Precondición: no tiene
loMismo x = x

siempreSiete :: a-> Int
--Dado un elemento de algún tipo devuelve el número 7.
--Precondición: no tiene
siempreSiete x = 7

swap :: (a,b)-> (b, a)
{-Dadas una tupla, invierte sus componentes.
¿Por qué existen dos variables de tipo diferentes?
Existen dos variables de tipo (a y b) para permitir que los componentes de
la tupla sean de tipos distintos entre sí (por ejemplo, (String, Int)). 
Si usáramos la misma variable (a, a), la función quedaría restringida 
únicamente a tuplas con elementos del mismo tipo, perdiendo generalidad.-}
--Precondición: no tiene
swap (x,y) = (y,x)

{- 2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimór cas?
Son polimórficas (específicamente polimorfismo paramétrico) porque su 
lógica es independiente de los tipos de datos que reciban. La función no 
opera sobre el contenido de los valores, sino sobre la estructura, 
tratándolos como "cajas negras". -}

{-6. Pattern matching sobre listas
1. De na las siguientes funciones polimór cas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):-}

estaVacia :: [a]-> Bool
{-Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
De nida en Haskell como null.-}
--Precondición: no tiene
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a]-> a
{-Dada una lista devuelve su primer elemento.
De nida en Haskell como head.
Precondición: la lista no puede estar vacía.
Nota: tener en cuenta que el constructor de listas es :-}
elPrimero (x:_) = x
elPrimero (_) = error "La lista no puede estar vacía"

sinElPrimero :: [a]-> [a]
{-Dada una lista devuelve esa lista menos el primer elemento.
De nida en Haskell como tail.
Precondición: la lista no puede estar vacía.
Nota: tener en cuenta que el constructor de listas es :-}
sinElPrimero (x:xs) = xs
sinElPrimero (_) = error "La lista no puede estar vacía"

splitHead :: [a]-> (a, [a])
{-Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero.
Precondición: la lista no puede estar vacía.
Nota: tener en cuenta que el constructor de listas es :-}
splitHead (x:xs) = (x, xs)
splitHead (_) = error "La lista no puede estar vacía"

