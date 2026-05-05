{-1. Pizzas
Tenemos los siguientes tipos de datos:-}

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

p0 = Prepizza
p1 = Capa Salsa p0
p2 = Capa Queso p1
p3 = Capa Jamon p2
p4 = Capa (Aceitunas 8) p3
p5 = Capa (Aceitunas 2) p4

--De�nir las siguientes funciones:

cantidadDeCapas :: Pizza -> Int
{- Dada una pizza devuelve la cantidad de ingredientes
Precondición: ninguna. -}
cantidadDeCapas Prepizza =  0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
{- Dada una lista de ingredientes construye una pizza
Precondición: ninguna -}
armarPizza [] = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)

sacarJamon :: Pizza -> Pizza
{- Le saca los ingredientes que sean jamón a la pizza
Precondición: ninguna. -}
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = 
  if esJamon ing
  then sacarJamon p
  else Capa ing (sacarJamon p)

esJamon :: Ingrediente -> Bool
{- Dado un ingrediente indica si es Jamon o no.
Precondición: ninguna. -}
esJamon Jamon = True
esJamon _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
{- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
Precondición:  -}
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = esSalsaOQueso ing && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

duplicarAceitunas :: Pizza -> Pizza
{- Recorre cada ingrediente y si es aceitunas duplica su cantidad
Precondición: ninguna -}
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarSiEsAceituna ing) (duplicarAceitunas p)

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
{- Dado un Ingrediente, si es Aceituna le duplica la cantidad, si no, lo retorna tal cual es.
Precondición: ninguna. -}
duplicarSiEsAceituna (Aceitunas n) = Aceitunas (n*2)
duplicarSiEsAceituna ing = ing

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
{- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.
Precondicón: ninguna -}
cantCapasPorPizza [] = []
cantCapasPorPizza (cc:ccs) = (contarCapas cc, cc) : cantCapasPorPizza ccs

contarCapas :: Pizza -> Int
{- Dada una Pizza retorna la cantidad de ingredientes que tiene.
Precondición: ninguna -}
contarCapas Prepizza = 0
contarCapas (Capa _ p) = 1 + contarCapas p

--------------------

{- 2. Mapa de tesoros (con bifurcaciones)
Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.-}
data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

ob0 = []
ob1 = [Chatarra]
ob2 = [Tesoro]
ob3 = ob1++ob2
ob4 = ob2++ob2
ob5 = ob3++ob2

c0 = Cofre ob0
c1 = Cofre ob1
c2 = Cofre ob2
c3 = Cofre ob3
c4 = Cofre ob4
c5 = Cofre ob5

m0 = Fin c0
m1 = Fin c1
m2 = Fin c2
m3 = Bifurcacion c3 m0 m1
m4 = Bifurcacion c4 m1 m2
m5 = Bifurcacion c5 m1 m4
m6 = Bifurcacion c1 m0 m2
m7 = Bifurcacion c1 m4 m4
m8 = Bifurcacion c4 m7 m1
m9 = Bifurcacion c2 m5 m8
-----
--Definir las siguientes operaciones:
hayTesoro :: Mapa-> Bool
{-Indica si hay un tesoro en alguna parte del mapa.
Precondición: tiene que existir al menos un objeto en cada cofre. -}
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c mi md) = tieneTesoro c || hayTesoro mi || hayTesoro md

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre []) = error "Tiene que existir al menos un objeto"
tieneTesoro (Cofre objs) = existeTesoro objs

existeTesoro :: [Objeto] -> Bool
{- Dada una lista de tipo Objeto, indica si contiene al menos un Tesoro.
Precondición: ninguna. -}
existeTesoro [] = False
existeTesoro (obj:objs) = esTesoro obj || existeTesoro objs

esTesoro :: Objeto -> Bool
{- Dado un Objeto, indica si es un Tesoro.
Precondición: ninguna. -}
esTesoro Tesoro = True
esTesoro _ = False
-----

hayTesoroEn :: [Dir]-> Mapa-> Bool
{-Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
lista vacía de direcciones.
Precondición: ninguna -}
hayTesoroEn [] m = nodoTieneTesoro m
hayTesoroEn (dir:dirs) m = hayTesoroEn dirs (desviarCamino dir m)

nodoTieneTesoro :: Mapa -> Bool
{- Dado un Mapa indica si el nodo de dicho mapa tiene un tesoro.
Precondición: ninguna. -}
nodoTieneTesoro (Fin c) = tieneTesoro c
nodoTieneTesoro (Bifurcacion c _ _) = tieneTesoro c

desviarCamino :: Dir -> Mapa -> Mapa
{- Dada una Dir y un Mapa, retorna la rama del mapa según la dirección dada.
Precondición: el mapa debe ser una Bifurcación. -}
desviarCamino Izq (Bifurcacion _ mi _) = mi
desviarCamino Der (Bifurcacion _ _ md) = md
desviarCamino _ _ = error "El camino excede el límite del mapa dado"
-----

caminoAlTesoro :: Mapa-> [Dir]
{- Indica el camino al tesoro.
Precondición: existe un tesoro y es único.  -}
caminoAlTesoro (Fin c) =
  if tieneTesoro c
    then []
    else error "Debe existir al menos un tesoro en el mapa"
caminoAlTesoro (Bifurcacion _ mi md) = 
  if hayTesoro mi
    then Izq : caminoAlTesoro mi
    else if hayTesoro md
      then Der : caminoAlTesoro md
      else error "Debe existir al menos un tesoro en el mapa"
-----

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
{-Indica el camino de la rama más larga.
Precondición: ninguna -}
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion _ mi md) = 
	if length(caminoDeLaRamaMasLarga mi) > length(caminoDeLaRamaMasLarga md)
	then Izq : caminoDeLaRamaMasLarga mi
	else Der : caminoDeLaRamaMasLarga md
-----

tesorosPorNivel :: Mapa -> [[Objeto]]
{-Devuelve los tesoros separados por nivel en el árbol.
Precondición: ninguna -}
tesorosPorNivel (Fin c) = [tesorosDe c]
tesorosPorNivel (Bifurcacion c mi md) = 
	tesorosDe c : tesorosPorNivel mi ++ tesorosPorNivel md
	
tesorosDe :: Cofre -> [Objeto]
{- Dado un Cofre, devuelve una lista de los tesoros que tiene.
Precondición: ninguna. -}
tesorosDe (Cofre obs) = obtenerTesoros obs

obtenerTesoros :: [Objeto] -> [Objeto]
{- Dada una lista de Objeto, retorna una lista con los que son Tesoro.
Precondición: ninguna. -}
obtenerTesoros [] = []
obtenerTesoros (ob:obs) =
	if esTesoro ob
	then ob : obtenerTesoros obs
	else obtenerTesoros obs
-----

todosLosCaminos :: Mapa -> [[Dir]]
{- Devuelve todos lo caminos en el mapa.
Precondición: ninguna. -}
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ mi md) = agregarIzq (todosLosCaminos mi) ++ agregarDer (todosLosCaminos md)

agregarIzq :: [[Dir]] -> [[Dir]]
{- Dada una lista de caminos, agrega la dirección Izq al inicio de cada camino.
Precondición: ninguna. -}
agregarIzq [] = []
agregarIzq (cam:cams) = (Izq : cam) : agregarIzq cams


agregarDer :: [[Dir]] -> [[Dir]]
{- Dada una lista de caminos, agrega la dirección Der al inicio de cada camino.
Precondición: ninguna. -}
agregarDer [] = []
agregarDer (cam:cams) = (Der : cam) : agregarDer cams
-----

{- 3. Nave Espacial
modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:-}
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

b0 = []
b1 = [Comida, Oxigeno]
b2 = b1++[Torpedo, Oxigeno]

trs0 = []
trs1 = "Leo" : trs0
trs2 = "Elias" : trs1
trs3 = "Montiel" : trs2
trs4 = trs1 ++ trs3

s0 = S "0" [] trs0
s1 = S "1" [LanzaTorpedos] trs0
s2 = S "2" [Motor 8] trs1
s3 = S "3" [Almacen b0] trs2
s4 = S "4" [Motor 20, Almacen b1] trs3
s5 = S "5" [Almacen b1, Almacen b2] trs4

ts0 = NodeT s0 EmptyT EmptyT
ts1 = NodeT s1 ts0 EmptyT
ts2 = NodeT s2 ts1 ts0
ts3 = NodeT s3 ts1 ts2
ts4 = NodeT s4 ts2 ts3
ts5 = NodeT s5 ts3 ts3

n0 = N (ts0)
n1 = N (ts1)
n2 = N (ts2)
n3 = N (ts3)
n4 = N (ts4)
n5 = N (ts5)

--Implementar las siguientes funciones utilizando recursión estructural:
sectores :: Nave -> [SectorId]
{-Propósito: Devuelve todos los sectores de la nave.
Precondición: ninguna -}
sectores (N ts) = sinRepetidos (sectoresIdDe ts)

sectoresIdDe :: Tree Sector -> [SectorId]
{- Dado un árbol de Sector, retorna una lista con todos los SectorId del árbol.
Precondición: ninguna -}
sectoresIdDe EmptyT = []
sectoresIdDe (NodeT s tsi tsd) = sectorId s : (sectoresIdDe tsi ++ sectoresIdDe tsd)

sectorId :: Sector -> SectorId
{- Dado un Sector, retorna su respectivo SectorId.
Precondición: ninguna. -}
sectorId (S sid _ _) = sid

sinRepetidos :: Eq a => [a] -> [a]
{- Dada una lista de elementos, devuelve dicha lista sin sus elementos repetidos.
Precondición: ninguna. -}
sinRepetidos [] = []
sinRepetidos (x:xs) =
  if pertenece x xs
    then sinRepetidos xs
    else x : sinRepetidos xs

pertenece :: Eq a => a-> [a]-> Bool
{-Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.
Precondición: ninguna. -}
pertenece _ [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs
-----

poderDePropulsion :: Nave -> Int
{- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
el poder de propulsión es el número que acompaña al constructor de motores.
Precondición: ninguna. -}
poderDePropulsion (N ts) = totalDePoderDePropulsion ts

totalDePoderDePropulsion :: Tree Sector -> Int
{- Dado un árbol de Sector, indica el poder total de propulsión que hay en todo el árbol.
Precondición: ninguna. -}
totalDePoderDePropulsion EmptyT = 0
totalDePoderDePropulsion (NodeT s tsi tsd) = cantPoderDePropulsionDe s + totalDePoderDePropulsion tsi + totalDePoderDePropulsion tsd

cantPoderDePropulsionDe :: Sector -> Int
{- Dado un Sector, indica la cantidad de poder de propulsión que tiene tal sector.
Precondición: ninguna. -}
cantPoderDePropulsionDe (S _ comps _) = cantPoderDePropulsion comps

cantPoderDePropulsion :: [Componente] -> Int
{- Dada una lista de Componente, indica la cantidad de poder de propulsión que hay en ella.
Precondición: ninguna. -}
cantPoderDePropulsion [] = 0
cantPoderDePropulsion (comp:comps) = cantPoderMotor comp + cantPoderDePropulsion comps

cantPoderMotor :: Componente -> Int
{- Dado un Componente, indica la cantidad de poder de propulsión que tiene si este fuera un Motor, sino retorna 0.
Precondición: ninguna. -}
cantPoderMotor (Motor n) = n
cantPoderMotor _ = 0
-----

barriles :: Nave -> [Barril]
{- Propósito: Devuelve todos los barriles de la nave.
Precondición: ninguna. -}
barriles (N ts) = sinRepetidosBarriles (barrilesEn ts)

barrilesEn :: Tree Sector -> [Barril]
{- Dado un árbol de Sector, retorna una lista con los Barriles que hay en él.
Precondición: ninguna. -}
barrilesEn EmptyT = []
barrilesEn (NodeT s ti td) =
  barrilesDel s ++ barrilesEn ti ++ barrilesEn td

barrilesDel :: Sector -> [Barril]
{- Dado un Sector, retorna una lista con los Barriles que hay en él.
Precondición: ninguna. -}
barrilesDel (S _ comps _) = concatMap barrilesDeComponente comps

barrilesDeComponente :: Componente -> [Barril]
{- Dado un Componente, retorna una lista con sus Barriles si es un Almacen.
Precondición: ninguna. -}
barrilesDeComponente (Almacen bs) = bs
barrilesDeComponente _ = []

sinRepetidosBarriles :: [Barril] -> [Barril]
{- Dada una lista de Barril, la retorna sin los barriles que se repiten.
Precondición: ninguna. -}
sinRepetidosBarriles [] = []
sinRepetidosBarriles (b:bs) = 
  if perteneceBarrilA b bs
    then sinRepetidosBarriles bs
    else b : sinRepetidosBarriles bs

perteneceBarrilA :: Barril -> [Barril] -> Bool
{- Dado un Barril y una lista de Barril, indica si el elemento pertenece a la lista.
Precondición: ninguna. -}
perteneceBarrilA _ [] = False
perteneceBarrilA b (b':bs) = esMismoBarril b b' || perteneceBarrilA b bs

esMismoBarril :: Barril -> Barril -> Bool
{- Dado dos elementos de tipo Barril, indica si son lo mismo.
Precondición: ninguna. -}
esMismoBarril Comida Comida = True
esMismoBarril Oxigeno Oxigeno = True
esMismoBarril Torpedo Torpedo = True
esMismoBarril Combustible Combustible = True
esMismoBarril _ _ = False
-----

agregarASector :: [Componente]-> SectorId-> Nave-> Nave
{- Propósito: Añade una lista de componentes a un sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no añade componentes
Precondición: ninguna. -}
agregarASector [] _ n = n
agregarASector comps sid (N ts) = N (agregarASectorEn comps sid ts)

agregarASectorEn :: [Componente]-> SectorId-> Tree Sector -> Tree Sector
{- Dada una lista de Componente, un SectorId y un árbol de Sector, retorna dicho árbol
con los componentes listados ya agregados a él, si existiera el id del sector dado.
Precondición: ninguna. -}
agregarASectorEn _ _ EmptyT = EmptyT
agregarASectorEn comps sid (NodeT s si sd) =
  if tieneMismoId sid s
    then NodeT (agregarComponentes comps s) si sd
    else NodeT s (agregarASectorEn comps sid si) (agregarASectorEn comps sid sd)

tieneMismoId :: SectorId -> Sector -> Bool
{- Dado un SectorId y un Sector, indica si dicho sector tiene el mismo id que el dado.
Precondición: ninguna. -}
tieneMismoId sid (S sid' _ _) = sid == sid'

agregarComponentes :: [Componente]-> Sector-> Sector
{- Dada una lista de Componente y un Sector, retorna dicho Sector con los componentes ya agregados.
Precondición: ninguna. -}
agregarComponentes comps (S id comps' ts) = S id (comps++comps') ts
-----

asignarTripulanteA :: Tripulante-> [SectorId]-> Nave-> Nave
{- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: Todos los id de la lista existen en la nave. -}
asignarTripulanteA t sids n =
  if verificarIdsExisten sids n
    then asignarTripulanteAImpl t sids n
    else error "Precondición violada: algunos IDs no existen en la nave"

asignarTripulanteAImpl :: Tripulante-> [SectorId]-> Nave-> Nave
{- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: ninguna. -}
asignarTripulanteAImpl _ [] n = n
asignarTripulanteAImpl t sids (N ts) = N (asignarTripulanteEn t sids ts)

asignarTripulanteEn :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
{- Propósito: Dado un Tripulante, una lista de SectorId y un árbol de Sector, 
retorna dicho árbol con el Tripulante agregado a sus respectivos sectores.
Precondición: ninguna. -}
asignarTripulanteEn _ _ EmptyT = EmptyT
asignarTripulanteEn t sids (NodeT s si sd) =
  NodeT (agregarTripulanteSiCoincideId t s sids) (asignarTripulanteEn t sids si) (asignarTripulanteEn t sids sd)

agregarTripulanteSiCoincideId :: Tripulante -> Sector -> [SectorId] -> Sector
{- Propósito: Dado un Tripulante, un Sector y una lista de SectorId, 
retorna dicho Sector con el Tripulante agregado si su id coincide con los de la lista.
Precondición: ninguna. -}
agregarTripulanteSiCoincideId t s sids =
  if existeIdEn s sids
    then agregarTripulante t s
    else s

existeIdEn :: Sector -> [SectorId] -> Bool
{- Propósito: Dado un Sector y una lista de SectorId, indica si el id de tal sector está en la lista.
Precondición: ninguna. -}
existeIdEn (S id _ _) sids = pertenece id sids

agregarTripulante :: Tripulante -> Sector -> Sector
{- Propósito: Dado un Tripulante y un Sector, retorna dicho Sectir con el Tripulante agregado.
Precondición: ninguna. -}
agregarTripulante t (S id comps ts) = S id comps (t:ts)

verificarIdsExisten :: [SectorId] -> Nave -> Bool
{- Propósito: Dada una lista de SectorId y una Nave, indica si todos los Ids dados existen en ella.
Precondición: ninguna. -}
verificarIdsExisten [] _ = True
verificarIdsExisten (sid:sids) n = pertenece sid (sectores n) && verificarIdsExisten sids n
-----

sectoresAsignados :: Tripulante-> Nave-> [SectorId]
{- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
Precondición: ninguna. -}
sectoresAsignados t (N ts) = sinRepetidos (sectoresAsignadosEn t ts)

sectoresAsignadosEn :: Tripulante -> Tree Sector -> [SectorId]
{- Dado un Tripulante y un árbol de Sector, devuelve una lista con los SectorId asignados al tripulante
dentro del árbol.
Precondición: ninguna. -}
sectoresAsignadosEn _ EmptyT = []
sectoresAsignadosEn t (NodeT s si sd) = agregarIdAsignado t s ((sectoresAsignadosEn t si) ++ (sectoresAsignadosEn t si))

agregarIdAsignado :: Tripulante -> Sector -> [SectorId] -> [SectorId]
{- Dado un Tripulante, un Sector de la nave y una lista de SectorId, devuelve dicha lista
con el Id del sector dado solo si está asignado a él el tripulante.
Precondición: ninguna. -}
agregarIdAsignado t (S id _ ts) sids = if pertenece t ts then id : sids else sids
-----

tripulantes :: Nave-> [Tripulante]
{- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
Precondición: ninguna. -}
tripulantes (N ts) = sinRepetidos (tripulantesDe ts)

tripulantesDe :: Tree Sector -> [Tripulante]
{- Dado un árbol de Sector, retorna una lista con los tripulantes que estén en él.
Precondición: ninguna. -}
tripulantesDe EmptyT = []
tripulantesDe (NodeT s si sd) = tripulantesEn s ++ tripulantesDe si ++ tripulantesDe sd

tripulantesEn :: Sector -> [Tripulante]
{- Dado un Sector, retorna una lista con los tripulantes que estén en él.
Precondición: ninguna. -}
tripulantesEn (S _ _ ts) = ts
-----

{- 4. Manada de lobos
Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:

- Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
- Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
- Las crías poseen sólo un nombre y no poseen lobos a cargo.

La estructura es la siguiente:-}
type Presa = String-- nombre de presa
type Territorio = String-- nombre de territorio
type Nombre = String-- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

{- 1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso: -}
ps0 = []
ps1 = ["Venado", "Jabalí", "Paloma"]
ps2 = "Pez" : ps1

l0 = Cria "Tizi"
l1 = Cria "Lucia"
l2 = Cria "Leah"
l3 = Explorador "Mariana" ["Bernal", "Quilmes"] l0 l1
l4 = Explorador "Jorge" ["Quilmes", "Bernal", "Berazategui"] l2 l3
l5 = Cazador "Leo" ps0 l0 l1 l2
l6 = Cazador "Elias" ps1 l0 l1 l4
l7 = Cazador "Montiel" ps2 l2 l6 l4

ml0 = M l0
ml1 = M l1
ml2 = M l2
ml3 = M l3
ml4 = M l4
ml5 = M l5
ml6 = M l6
ml7 = M l7

buenaCaza :: Manada-> Bool
{-Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la
cantidad de crías.
Precondición: ninguna. -}
buenaCaza (M lobo) = buenaCazaDe lobo

buenaCazaDe :: Lobo -> Bool
{- Dado un Lobo, indica si la cantidad de alimento cazado es mayor a la
cantidad de crías.
Precondición: ninguna. -}
buenaCazaDe (Cazador _ ps l1 l2 l3) = length ps > (unoSi(esCria l1) + unoSi(esCria l2) + unoSi(esCria l3))
buenaCazaDe _ = False

esCria :: Lobo -> Bool
{- Dado un Lobo, indica si es una Cría o no.
Precondición: ninguna. -}
esCria (Cria _) = True
esCria _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0
-----

elAlfa :: Manada-> (Nombre, Int)
{-Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.
Precondición: ninguna. -}
elAlfa (M lobos) = elAlfaDe lobos

elAlfaDe :: Lobo -> (Nombre, Int)
{- Dado un Lobo, analiza tanto a él como a sus lobos a cargo y devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas.
Precondición: ninguna. -}
elAlfaDe (Cria n) = (n, 0)
elAlfaDe (Explorador _ _ l0 l1) = alfaEntre (elAlfaDe l0) (elAlfaDe l1)
elAlfaDe (Cazador n ps l0 l1 l2) = alfaEntre ((n, length ps)) (alfaEntre (elAlfaDe l0) (alfaEntre (elAlfaDe l1) (elAlfaDe l2)))

alfaEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
{- Dado dos pares con nombre de lobo y cantidad de presas casadas, devuelve la que tenga mayor cantidad.
Precondición: ninguna. -}
alfaEntre (n1, ps1) (n2, ps2) = if ps1 > ps2 then (n1, ps1) else (n2, ps2)
-----

losQueExploraron :: Territorio-> Manada-> [Nombre]
{- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio
Precondición: ninguna. -}
losQueExploraron t (M lobos) = sinRepetidos (losQueExploraronEn t lobos)

losQueExploraronEn :: Territorio -> Lobo -> [Nombre]
{- Dado un Territorio y un Lobo, devuelve los nombres de los exploradores que
pasaron por dicho territorio
Precondición: ninguna.  -}
losQueExploraronEn t (Explorador n ts l1 l2) = let exploradores = (losQueExploraronEn t l1) ++ (losQueExploraronEn t l2)
  in agregarExplorador t ts n exploradores
losQueExploraronEn t (Cazador _ _ l1 l2 l3) = (losQueExploraronEn t l1) ++ (losQueExploraronEn t l2) ++ (losQueExploraronEn t l3)
losQueExploraronEn _ _ = []

agregarExplorador :: Territorio -> [Territorio] -> Nombre -> [Nombre] -> [Nombre]
{- Dado un Territorio, una lista de Territorio explorados, un nombre de explorador y una lista de nombres de exploradores,
retorna dicha lista con el nombre incluído del explorador si es que exploró el territorio dado.
Precondición: ninguna. -}
agregarExplorador t ts n exps =
  if pertenece t ts
    then n : exps
    else exps
-----

exploradoresPorTerritorio :: Manada-> [(Territorio, [Nombre])]
{- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un terri
torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.
Precondición: ninguna. -}
exploradoresPorTerritorio (M lobo) = sinRepetidos (exploradoresPorTerritorioDe lobo)

exploradoresPorTerritorioDe :: Lobo -> [(Territorio, [Nombre])]
{- Propósito: dado un Lobo, denota la lista de los pares cuyo primer elemento es un terri
torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.
Precondición: ninguna. -}
exploradoresPorTerritorioDe (Explorador n ts l1 l2) = 
  incluirExploradorATerritorios n ts (exploradoresPorTerritorioDe l1 ++ exploradoresPorTerritorioDe l2)
exploradoresPorTerritorioDe (Cazador _ _ l1 l2 l3) = (exploradoresPorTerritorioDe l1 ++ exploradoresPorTerritorioDe l2 ++ exploradoresPorTerritorioDe l3)
exploradoresPorTerritorioDe _ = []

incluirExploradorATerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
{- Propósito: dado un Nombre, una la lista de Territorio, y una lista de los pares cuyo primer elemento es un terri
torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Denota dicha lista pero con el nombre del Explorador agregado al territorio explorado.
Precondición: ninguna. -}
incluirExploradorATerritorios _ [] ttns = ttns
incluirExploradorATerritorios n (t:ts) ttns = incluirExploradorATerritorios n ts (agregarNombreSiExploro n t ttns)

agregarNombreSiExploro :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
{- Propósito: dado un Nombre, un Territorio y una lista de los pares cuyo primer elemento es un terri
torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Denota dicha lista pero con el nombre del Explorador agregado al territorio explorado.
Los territorios no deben repetirse.
Precondición: ninguna. -}
agregarNombreSiExploro n t [] = [(t,[n])]
agregarNombreSiExploro n t (ttns:ttnss) =
  if t == fst(ttns)
    then agregarNombre n ttns ttnss
    else ttns : (agregarNombreSiExploro n t ttnss)

agregarNombre :: Nombre -> (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
{- Dado un Nombre, un par cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los
nombres de los exploradores que exploraron dicho territorio, y una lista de dichos pares, denota la lista 
pero con el nombre del Explorador agregado al territorio explorado. -}
agregarNombre n (t,ns) ttnss = (t, n : ns) : ttnss
-----

cazadoresSuperioresDe :: Nombre-> Manada-> [Nombre]
{-Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores
que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el subordinado
de un subordinado).
Precondición: hay un lobo con dicho nombre y es único.
Eficiencia: O(n), donde n es el número de lobos en la manada, ya que revisa cada lobo una sola vez. -}
cazadoresSuperioresDe n m =
  if existeLobo n m
    then cazadoresSuperioresEn n (extraerLobo m)
    else error "Precondición violada: el lobo no existe en la manada"

existeLobo :: Nombre -> Manada -> Bool
existeLobo n (M lobo) = estaEnLobo n lobo

extraerLobo :: Manada -> Lobo
extraerLobo (M lobo) = lobo

cazadoresSuperioresEn :: Nombre -> Lobo -> [Nombre]
{- Dado un Nombre y un Lobo, indica el nombre de todos los cazadores
que tienen como subordinado al lobo dado.
Precondición: hay un lobo con dicho nombre y es único. -}
cazadoresSuperioresEn _ (Cria _) = []
cazadoresSuperioresEn n (Explorador ne _ l1 l2) =
  if n == ne
    then []
    else cazadoresSuperioresEn n l1 ++ cazadoresSuperioresEn n l2
cazadoresSuperioresEn n (Cazador nc _ l1 l2 l3) =
  if n == nc
    then []
    else
      if estaEnLobo n l1 || estaEnLobo n l2 || estaEnLobo n l3
        then nc : (cazadoresSuperioresEn n l1 ++ cazadoresSuperioresEn n l2 ++ cazadoresSuperioresEn n l3)
        else []

estaEnLobo :: Nombre -> Lobo -> Bool
{- Dado un Nombre y un Lobo, indica si el nombre es está en la jerarquía del lobo. -}
estaEnLobo n (Cria nombre) = n == nombre
estaEnLobo n (Explorador nombre _ l1 l2) = n == nombre || (estaEnLobo n l1 || estaEnLobo n l2)
estaEnLobo n (Cazador nombre _ l1 l2 l3) = n == nombre || (estaEnLobo n l1 || estaEnLobo n l2 || estaEnLobo n l3)

--Suponiendo la siguiente manada de ejemplo:

manadaEj = M (Cazador "DienteFiloso" ["Búfalos", "Antílopes"] (Cria "Hopito")
  (Explorador "Incansable" ["Oeste hasta el río"]
    (Cria "MechónGris")
    (Cria "Rabito")
  )
  (Cazador "Garras" ["Antílopes", "Ciervos"]
    (Explorador "Zarpado" ["Bosque este"]
      (Cria "Osado")
      (Cazador "Mandíbulas" ["Cerdos", "Pavos"]
        (Cria "Desgreñado")
        (Cria "Malcriado")
        (Cazador "TrituraHuesos" ["Conejos"]
          (Cria "Peludo")
          (Cria "Largo")
          (Cria "Menudo")
        )
      )
    )
    (Cria "Garrita")
    (Cria "Manchas")
  ))
{-la función cazadoresSuperioresDe debería dar lo siguiente:
cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
cazadoresSuperioresDe "DienteFiloso" manadaEj = []
cazadoresSuperioresDe "Peludo" manadaEj =
["DienteFiloso", "Garras", "Mandíbulas", "TrituraHuesos"]
-}