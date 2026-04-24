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



