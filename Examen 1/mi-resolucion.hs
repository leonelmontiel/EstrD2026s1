{-Un árbol genealógico puede representarse de alguna forma mediantes tipos algebraicos.
En este ejercicio se desarrolla un tipo para expresar esta idea, y se pide definir algunas funciones que manipulan árboles genealógicos.
Los tipos que representan esta idea son los siguientes:-}

type Nombre = String

data EstadoCivil =
        SinPareja
    | EnMatrimonioCon Nombre
    | EnMatrimonioConPeroHijosCon Nombre Nombre deriving Show
data Persona = P Nombre EstadoCivil deriving Show
data GenTree =
        Hijos0 Persona
    | Hijos1 Persona GenTree
    | Hijos2 Persona GenTree GenTree
    | Hijos3 Persona GenTree GenTree GenTree
{-Las personas se representan mediante un nombre y un estado civil.
El árbol genealógico expresa quiénes son todos los hijos de una persona específica, que a su vez indican sus propios árboles genealógicos; los diferentes 
constructores expresan personas con diferente cantidad de hijos (siendo Hijos0 el caso de una persona sin hijos, etc.).
Para la resolución de los ejercicios, puede resultar útil usar las siguientes funciones:-}


elegirEntre :: Maybe a -> Maybe a -> Maybe a
elegirEntre (Just x) m = Just x
elegirEntre Nothing m = m

elegirSi :: Bool -> Maybe a -> Maybe a -> Maybe a
elegirSi True  m1 m2 = m1
elegirSi False m1 m2 = m2

agregar_A_ :: a -> Maybe [a] -> Maybe [a]
agregar_A_ y Nothing = Nothing
agregar_A_ y (Just xs) = Just (y : xs)
--A modo de ejemplo, se representan (parcialmente) los árboles genealógicos de las familias Malfoy y Black.


brutus     = P "Brutus Malfoy II" (EnMatrimonioCon "Cierce Vyndon")
pollux     = P "Pollux Black" (EnMatrimonioCon "Irma Crabbe")
abraxas    = P "Abraxas Malfoy" (EnMatrimonioCon "(no identificada)")
walburga   = P "Walburga Black" (EnMatrimonioCon "Orion Black")
alphard    = P "Alphard Black" SinPareja
cygnus     = P "Cygnus Black III" (EnMatrimonioCon "Druella Rossier")
regulus    = P "Regulus Black" SinPareja
sirius     = P "Sirius Black" SinPareja
bellatrix  = P "Bellatrix Black" (EnMatrimonioConPeroHijosCon "Rodolphus Lestrange" "Tom Malvoro Riddle")
andromeda  = P "Andromeda Black" (EnMatrimonioCon "Edward Tonks")
narcissa   = P "Narcissa Black" (EnMatrimonioCon "Lucius Malfoy")
lucius     = P "Lucius Malfoy" (EnMatrimonioCon "Narcissa Black")
nymphadora = P "Nymphadora Tonks" (EnMatrimonioCon "Remus Lupin")
delphini   = P "Delphini" SinPareja
edward     = P "Edward Remus Lupin" SinPareja
draco      = P "Draco Malfoy" (EnMatrimonioCon "Astoria Greengrass")
scorpius   = P "Scorpius Malfoy" (EnMatrimonioCon "Lilly Potter")

fliaBlack = Hijos3 pollux
              (Hijos2 walburga (Hijos0 regulus) (Hijos0 sirius))
              (Hijos0 alphard)
              (Hijos3 cygnus
                 (Hijos1 bellatrix (Hijos0 delphini))
                 (Hijos1 andromeda (Hijos1 nymphadora (Hijos0 edward)))
                 (Hijos1 narcissa (Hijos1 draco (Hijos0 scorpius))))
               
fliaMalfoy = Hijos1 brutus
               (Hijos1 abraxas
                  (Hijos1 lucius (Hijos1 draco (Hijos0 scorpius))))
{-Ejercicio 1 (Preparación)
Escribir la función cabezaDeFamiliaDe :: GenTree -> Persona que describe la persona que encabeza el árbol genealógico dado

EJEMPLOS:

cabezaDeFamiliaDe fliaBlack = P "Pollux Black" (EnMatrimonioCon "Irma Crabbe")
cabezaDeFamiliaDe fliaMalfoy = P "Brutus Malfoy II" (EnMatrimonioCon "Cierce Vyndon")-}

cabezaDeFamiliaDe :: GenTree -> Persona
{- Dado un GenTree, describe la persona que encabeza el árbol genealógico dado.
Precondición: ninguna. -}
cabezaDeFamiliaDe (Hijos0 p) = p
cabezaDeFamiliaDe (Hijos1 p _) = p
cabezaDeFamiliaDe (Hijos2 p _ _) = p
cabezaDeFamiliaDe (Hijos3 p _ _ _) = p

---------

{-Ejercicio 2 (Precalentamiento)
Escribir la función hijosDe_En_ :: Nombre -> GenTree -> Maybe [Persona] que describe los hijos de la persona con nombre dado, o Nothing si ninguna persona del árbol tiene ese nombre.
Puede suponerse (sin verificar) que los nombres de las personas son únicos en el árbol dado.

EJEMPLOS:


hijosDe "Cygnus Black III" fliaBlack
   = Just [ P "Bellatrix Black" (EnMatrimonioConPeroHijosCon "Rodolphus Lestrange" "Tom Malvoro Riddle")
          , P "Andromeda Black" (EnMatrimonioCon "Edward Tonks")
          , P "Narcissa Black" (EnMatrimonioCon "Lucius Malfoy")
          ]
          
hijosDe "Sirius Black" fliaBlack = Just []

hijosDe "Abraxas Malfoy" fliaMalfoy
   = Just [ P "Lucius Malfoy" (EnMatrimonioCon "Narcissa Black") ]

hijosDe "Narcissa Black" fliaBlack
   = Just [ P "Draco Malfoy" (EnMatrimonioCon "Astoria Greengrass") ]

hijosDe "Narcissa Black" fliaMalfoy = Nothing

hijosDe "Draco Malfoy" fliaBlack
   = Just [ P "Scorpius Malfoy" (EnMatrimonioCon "Lilly Potter") ]

hijosDe "Draco Malfoy" fliaMalfoy
   = Just [ P "Scorpius Malfoy" (EnMatrimonioCon "Lilly Potter") ]

hijosDe "James Potter" fliaBlack = Nothing-}

hijosDe_En_ :: Nombre -> GenTree -> Maybe [Persona]
{- Dado un Nombre y un GenTree, describe los hijos de la persona con nombre dado, o Nothing si ninguna persona del árbol tiene ese nombre.
Puede suponerse (sin verificar) que los nombres de las personas son únicos en el árbol dado.
Precondición: ninguna. -}
hijosDe_En_ n gt =
    if n == nombre (cabezaDeFamiliaDe gt)
       then Just (hijosDeRaiz gt)
       else buscarEnLista n (subarbolesDe gt)

hijosDeRaiz :: GenTree -> [Persona]
{- Dado un GenTree, retorna las personas raíz de una lista de subárboles.
Precondición: ninguna. -}
hijosDeRaiz gt = personasDe (subarbolesDe gt)

subarbolesDe :: GenTree -> [GenTree]
{- Dado un GenTree, retorna una lista con los subárboles genealógicos según el caso concreto.
Precondición: ninguna. -}
subarbolesDe (Hijos0 _)           = []
subarbolesDe (Hijos1 _ g1)        = [g1]
subarbolesDe (Hijos2 _ g1 g2)     = [g1, g2]
subarbolesDe (Hijos3 _ g1 g2 g3)  = [g1, g2, g3]

personasDe :: [GenTree] -> [Persona]
{- Dada una lista de GenTree, retorna una lista con las Personas cabeza de familia de cada árbol genealogico contenido allí.
Precondición: ninguna. -}
personasDe []     = []
personasDe (g:gs) = cabezaDeFamiliaDe g : personasDe gs

buscarEnLista :: Nombre -> [GenTree] -> Maybe [Persona]
{- Dado un Nombre y una lista de GenTree, describe los hijos de la persona con nombre dado, o Nothing si ninguna persona del árbol tiene ese nombre.
Precondición: ninguna. -}
buscarEnLista _ []     = Nothing
buscarEnLista n (g:gs) = elegirEntre (hijosDe_En_ n g) (buscarEnLista n gs)

nombre :: Persona -> String
{- Dada una Persona, retorna su nombre.
Precondición: ninguna. -}
nombre (P n _) = n

----------

{- Ejercicio 3 (El interesante)
Escribir la función ancestrosDe_En_ :: Nombre -> GenTree -> [Persona] que describe los ancestros de la persona con nombre dado, o falla con error 
si ninguna persona del árbol tiene ese nombre.
Puede suponerse (sin verificar) que los nombres de las personas son únicos en el árbol dado.

EJEMPLOS:

ancestrosDe_En_ "Draco Malfoy" fliaBlack 
  = [ 
      P "Pollux Black" (EnMatrimonioCon "Irma Crabbe")
    , P "Cygnus Black III" (EnMatrimonioCon "Druella Rossier")
    , P "Narcissa Black" (EnMatrimonioCon "Lucius Malfoy")
    ]

ancestrosDe_En_ "Draco Malfoy" fliaMalfoy
  = [ 
      P "Brutus Malfoy II" (EnMatrimonioCon "Cierce Vyndon")
    , P "Abraxas Malfoy" (EnMatrimonioCon "(no identificada)")
    , P "Lucius Malfoy" (EnMatrimonioCon "Narcissa Black")
    ]

ancestrosDe_En_ "Pollux Black" fliaBlack = []

ancestrosDe_En_ "Harry Potter" fliaBlack = error "No pertenece a la familia"

ancestrosDe_En_ "Lucius Malfoy" fliaBlack = error "No pertenece a la familia"

ancestrosDe_En_ "Lucius Malfoy" fliaMalfoy
  = [
      P "Brutus Malfoy II" (EnMatrimonioCon "Cierce Vyndon")
    , P "Abraxas Malfoy" (EnMatrimonioCon "(no identificada)")
    ]
-}

ancestrosDe_En_ :: Nombre -> GenTree -> [Persona]
{- Dado un Nombre y un GenTree, describe los ancestros de la persona con nombre dado, o falla con error 
si ninguna persona del árbol tiene ese nombre.
Puede suponerse (sin verificar) que los nombres de las personas son únicos en el árbol dado.
Precondición: debe existir alguna persona del árbol genealógico con el nombre dado. -}
ancestrosDe_En_ n gt =
    case ancestrosDe_En_Maybe n gt of
        Just ancestros -> ancestros
        Nothing -> error "No pertenece a la familia"

ancestrosDe_En_Maybe :: Nombre -> GenTree -> Maybe [Persona]
{- Dado un Nombre y un GenTree, devuelve Just la lista de ancestros si el nombre existe en el árbol;
si no existe, devuelve Nothing.
Precondición: ninguna. -}
ancestrosDe_En_Maybe n (Hijos0 p) =
    if n == nombre p
        then Just []
        else Nothing
ancestrosDe_En_Maybe n (Hijos1 p gt) =
    if n == nombre p
        then Just []
        else agregarAncestroSiPertenece p (ancestrosDe_En_Maybe n gt)
ancestrosDe_En_Maybe n (Hijos2 p gt1 gt2) =
    if n == nombre p
        then Just []
        else agregarAncestroSiPertenece p (buscarAncestrosEn n [gt1, gt2])
ancestrosDe_En_Maybe n (Hijos3 p gt1 gt2 gt3) =
    if n == nombre p
        then Just []
        else agregarAncestroSiPertenece p (buscarAncestrosEn n [gt1, gt2, gt3])

agregarAncestroSiPertenece :: Persona -> Maybe [Persona] -> Maybe [Persona]
{- Dada una Persona y un posible resultado de búsqueda, agrega la Persona al inicio de la lista
si el resultado es Just una lista; si es Nothing, mantiene Nothing.
Precondición: ninguna. -}
agregarAncestroSiPertenece _ Nothing = Nothing
agregarAncestroSiPertenece p (Just ancestros) = Just (p : ancestros)

buscarAncestrosEn :: Nombre -> [GenTree] -> Maybe [Persona]
{- Busca en una lista de subárboles el nombre dado y devuelve el primer resultado encontrado.
Precondición: ninguna. -}
buscarAncestrosEn _ [] = Nothing
buscarAncestrosEn n (gt:gts) =
    elegirEntre (ancestrosDe_En_Maybe n gt) (buscarAncestrosEn n gts)
