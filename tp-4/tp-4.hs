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

s0 = S "0" [] []
s1 = S "1" [LanzaTorpedos] []
s2 = S "2" [Motor 8] ["Leo"]
s3 = S "3" [Almacen b0] ["Leo", "Elias"]
s4 = S "4" [Motor 20, Almacen b1] ["Leo", "Elias", "Montiel"]
s5 = S "5" [Almacen b1, Almacen b2] ["Leo", "Elias", "Montiel"]

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

-- data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
-- data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
-- data Sector = S SectorId [Componente] [Tripulante]

-- type SectorId = String
-- type Tripulante = String

-- data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
-- data Nave = N (Tree Sector)

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