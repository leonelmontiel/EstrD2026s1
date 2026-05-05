data Maybe a = Nothing | Just a
data Estudiante = ConsEstudiante String [Int]

promedio :: Estudiante -> Maybe Int
promedio (ConsEstudiante _ notas) = 
  if null notas
  then Nothing
  else (Just calcularPromedio notas)
  
calcularPromedio :: [Int] -> Int
calcularPromedio [] = error "no hay notas para promediar"
calcularPromedio notas = div (sumarNotas notas) (lenght notas)

sumarNotas :: [Int] -> Int
sumarNotas [] = 0
sumarNotas (n:ns) = n + sumarNotas ns

-------

TABS:
Roles	-> Implementador
	-> Usuario
	-> Diseñador

Eficiencia	-> Constante O(1) / O(S+T)
		-> Lineal O(n) / O(S*T)
		-> Cuadrático O(n*2)
	