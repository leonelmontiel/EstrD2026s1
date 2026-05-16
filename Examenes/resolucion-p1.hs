data Persona = P Nombre EstadoCivil
data GenTree = Hijo0 Persona 
| Hijo1 Persona GenTree 
| Hijo2 Persona GenTree GenTree
| Hijo3 Persona GenTree GenTree GenTree

elegirEntre :: Maybe a -> Maybe a -> Maybe a
elegirSi :: Bool -> Maybe a -> Maybe a -> Maybe a
agregarA :: a -> Maybe [a] -> Maybe [a]

-----

patriarcaDe :: GenTree -> Persona
patriarcaDe (Hijo0 p) = p
patriarcaDe (Hijo1 p _) = p
patriarcaDe (Hijo2 p _ _) = p
patriarcaDe (Hijo3 p _ _ _) = p

-----

hijosDe_En_ :: Nombre -> GenTree -> Maybe [Persona]
hijosDe_En_ n (Hijo0 p) = if n == nombreDe p then Just [] else Nothing
hijosDe_En_ n (Hijo1 p g) = 
  elegirSi (n == nombreDe p) (patriarcaDe g) (hijosDe_En_ p g)
hijosDe_En_ n (Hijo2 p g1 g2) = 
 elegirSi
  (n == nombreDe p) 
  (Just [patriarcaDe g1, patriarcaDe g2])
  (elegirEntre (hijosDe_En_ p g1) (hijosDe_En_ p g2))
hijosDe_En_ n (Hijo3 p g1 g2 g3) = 
  elegirSi
    (n == nombreDe p) 
    (Just [patriarcaDe g1, patriarcaDe g2, patriarcaDe g3])
    (elegirEntre (hijosDe_En_ p g1) 
      (elegirEntre 
        (hijosDe_En_ p g2) (hijosDe_En_ p g3)
      )
    )

nombreDe :: Persona -> Nombre
nombreDe (P n _) = n

-----

ancestrosDe_En_ :: Nombre -> GenTree -> [Persona]
ancestrosDe_En_ n g = 
  case ancestrosDe n g of
    Just ps -> ps
    Nothing -> error "El nombre debe estar en el árbol genealógico."

ancestrosDe :: Nombre -> GenTree -> Maybe [Persona]
ancestrosDe n (Hijos0 p) = if n == nombreDe p then Just [] else Nothing
ancestrosDe n (Hijos1 p g) =
 elegirSi
    (n == nombreDe p)
    (Just [])
    (agregarA p (ancestrosDe n g))
ancestrosDe n (Hijos2 p g1 g2) =
 elegirSi
    (n == nombreDe p)
    (Just [])
    (agregarA p (elegirEntre (ancestrosDe n g1) (ancestrosDe n g2)))
ancestrosDe n (Hijos3 p g1 g2 g3) =
  elegirSi
    (n == nombreDe p)
    (Just [])
    (agregarA p (elegirEntre (ancestrosDe n g1) (elegirEntre (ancestrosDe n g2) (ancestrosDe n g3))))




