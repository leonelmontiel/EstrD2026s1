{-1. Tipos recursivos simples
1.1. Celdas con bolitas
Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:-}
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
{-En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:-}
bol0 = CeldaVacia
bol1 = Bolita Rojo bol0
bol2 = Bolita Azul bol1
bol3 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
-- Implementar las siguientes funciones sobre celdas:

nroBolitas :: Color-> Celda-> Int
{-Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
existe una operación sobre listas que ayude a resolver el problema.
Precondición: ninguna. -}
nroBolitas _ CeldaVacia = 0
nroBolitas co1 (Bolita co2 ce) = unoSi (coincideColor co1 co2) + nroBolitas co1 ce

coincideColor :: Color -> Color -> Bool
{- Dados dos colores indica si son iguales o no.
Precondición: ninguna. -}
coincideColor Azul Azul = True
coincideColor Rojo Rojo = True
coincideColor _ _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0
----------

poner :: Color-> Celda-> Celda
{- Dado un color y una celda, agrega una bolita de dicho color a la celda.
Precondición: ninguna. -}
poner co CeldaVacia = Bolita co CeldaVacia
poner co ce = Bolita co ce
----------

sacar :: Color-> Celda-> Celda
{- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.
Precondición: ninguna. -}
sacar _ CeldaVacia = CeldaVacia
sacar co1 (Bolita co2 ce) = if coincideColor co1 co2 
                            then ce
                            else Bolita co2 (sacar co1 ce)
----------

ponerN :: Int-> Color-> Celda-> Celda
{- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
Precondición: ninguna. -}
ponerN 0 _ ce = ce
ponerN n c ce = if n>0 then poner c (ponerN (n-1) c ce) else ce
----------

{- 1.2. Camino hacia el tesoro
Tenemos los siguientes tipos de datos -}
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

c0 = Fin
c1 = Nada c0
c2 = Cofre [] c1
c3 = Nada c2
c4 = Cofre [Cacharro] c3
c5 = Cofre [Cacharro, Tesoro] c1
c6 = Nada c5
c7 = Nada (Nada (Cofre [] (Nada (Cofre [Tesoro, Tesoro] (Cofre [Cacharro, Tesoro] Fin)))))
--Definir las siguientes funciones:

hayTesoro :: Camino-> Bool
{- Indica si hay un cofre con un tesoro en el camino.
Precondición: ninguna. -}
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre objs c) = existeTesoro objs || hayTesoro c

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
----------

pasosHastaTesoro :: Camino-> Int
{- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro. -}
pasosHastaTesoro Fin = error "Tiene que haber al menos un tesoro."
pasosHastaTesoro (Nada c) = acumularPasos c
pasosHastaTesoro (Cofre objs c) = if not(existeTesoro objs) then acumularPasos c else 0

acumularPasos :: Camino -> Int
{- Dado un camino, suma 1 y retorna la cantidad de pasos contados.
Precondición: ninguna. -}
acumularPasos c = 1 + pasosHastaTesoro c
----------

hayTesoroEn :: Int-> Camino-> Bool
{- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.
Precondición: ninguna. -}
hayTesoroEn _ Fin = False
hayTesoroEn n (Nada c) = hayTesoroEnSiguientePaso n c
hayTesoroEn n (Cofre objs c) = if n == 0 then existeTesoro objs else hayTesoroEnSiguientePaso n c

hayTesoroEnSiguientePaso :: Int -> Camino -> Bool
{- Dada una cantidad de pasos y un Camino, indica si hay Tesoro en el paso siguiente.
Precondición: ninguna. -}
hayTesoroEnSiguientePaso n c = hayTesoroEn (n-1) c
----------

alMenosNTesoros :: Int-> Camino-> Bool
{- Indica si hay al menos n tesoros en el camino.
Precondición: ninguna. -}
alMenosNTesoros n Fin = n == 0
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = cantTesoros objs == n || alMenosNTesoros (n - cantTesoros objs) c

cantTesoros :: [Objeto] -> Int
{- Dada una lista de tipo Objeto, retorna la cantidad de Tesoros contenidos en ella.
Precondición: ninguna. -}
cantTesoros [] = 0
cantTesoros (obj:objs) = unoSi (esTesoro obj) + cantTesoros objs
----------

cantTesorosEntre :: Int-> Int-> Camino-> Int
{- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
Precondición: -}
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre n m (Nada c) =
    if m < 0
       then 0 -- Ya me pasé del rango, no sigo recorriendo
       else cantTesorosEntre (n-1) (m-1) c
cantTesorosEntre n m (Cofre objs c) =
    if m < 0
       then 0 -- Ya me pasé del rango
       else if n > 0
            then cantTesorosEntre (n-1) (m-1) c -- Todavía no llegué al rango
            else cantTesoros objs + cantTesorosEntre (n-1) (m-1) c -- Estoy dentro del rango