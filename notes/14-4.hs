data Tree a = EmptyT | NodeT a (Tree a) (Tree a) --arbol binario típico
t0 = EmptyT
t1 = NodeT 0 t0 t0
t2 = NodeT 2 t0 t1
t3 = NodeT 3 t1 t2
t4 = NodeT 4 t3 t3
t5 = NodeT 5 t0 t4

listPerLevel :: Tree a -> [[a]]
                       --   -
                       -- nivel
listPerLevel    EmptyT          = []
listPerLevel    (NodeT x ti td) = [x] : (combinar (listPerLevel ti)(listPerLevel td))

{-
          1 -------> n0 = [1] 
         / \
        2   3 -----> n1 = [2,3]
       /   / \
      4   8   5 ---> n2 = [4,8,5]

-}

combinar :: [[a]] -> [[a]] -> [[a]]
{- Dadas dos listas de listas, retorna una nueva lista de listas que las combina según elemento.
Precondición: ninguna. -}
combinar [] ys         = ys
combinar xs []         = xs
combinar (x:xs) (y:ys) = (x++y) : combinar xs ys

--------

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
            deriving Show
            
exp0 = Valor 0
exp1 = Valor 1
exp2 = Valor 5
exp3 = Sum exp0 exp0
exp4 = Sum exp0 exp1
exp5 = Sum exp2 exp1
exp6 = Prod exp0 exp0
exp7 = Prod exp0 exp1
exp8 = Prod exp1 exp2
exp9 = Sum exp8 exp7
exp10 = Neg exp3
exp11 = Neg exp10
exp12 = Neg exp9
exp13 = Neg (Sum (Neg exp2) exp0)
            
simplificar :: ExpA -> ExpA
simplificar (Valor x)  = Valor x
simplificar (Sum x y)  = simplificarSuma (simplificar x) (simplificar y)
simplificar (Prod x y) = simplificarProd (simplificar x) (simplificar y)
simplificar (Neg x)    = simplificarNeg (simplificar x)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) y = y
simplificarSuma x (Valor 0) = x
simplificarSuma x y         = Sum x y

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd x y         = Prod x y

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg x) = x
simplificarNeg x       = Neg x






