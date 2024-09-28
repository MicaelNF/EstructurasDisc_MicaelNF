--------------- Listas y recursión ---------------

longitud :: [a] -> Int --Muestra la cantidad de elementos que hay en una lista.
longitud [] = 0
longitud (x : xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a --Suma todos los elementos de una lista de tipo numérico.
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a] --Agrega un elemento a una lista ya sea al inicio o al final dependiendo del valor booleano.
agregaElemento lista elemento posInicio = --Reduje la función a un solo caso.
  if posInicio
    then elemento : lista
  else lista ++ [elemento]

maximoLista :: (Num a, Ord a) => [a] -> a --Calcula el valor máximo de una lista
maximoLista [] = error "Lista está vacia" --Cambie el caso base.
maximoLista [a] = a --Agregue un caso en el que solo hay un elemento por ende es el máximo.
maximoLista (x : xs) =
  if x > maximoLista(xs)
    then x
  else maximoLista(xs)

indice :: [a] -> Int -> a --Busca la posición en la que se encuntra cierto elemento en una lista a partir de un número entero dado.
indice [] numero = error "Lista esta vacia" --Agregue un caso base para una lista vacía.
indice (x : xs) numero =
  if (numero > 0) && (numero <= longitud(xs)) --No reste 1 ya que al quitar un elemento de la lista es como restarle uno.
    then indice(xs) (numero-1)
  else if numero == 0
    then x
  else error "Número no válido"

--------------- Listas por comprehensión ---------------
-------Elimine un caso base inncesario.-------
divisores :: Int -> [Int] --Esta función calcula y genera una lista de los números que pueden dividir a cierto numero dado.
divisores n = [x | x <- [1..n], mod n x == 0] --Use [1..n] para generar una lista sin depender de una función auxiliar.

conjunto :: Eq a => [a] -> [a] --Esta función genera una lista sin elementos repetidos.
conjunto [] = []
conjunto (x : xs) = x : conjunto[y | y <- xs, y /= x]
-- Pequeño ejemplo que use para verificar que estuviera bien mi recursión y entenderla mejor.
-- (1,2,2,3)
-- conjunto (1 : [2,2,3]) = 1 : conjunto[y | y <- [2,2,3], y /= x] = conjunto [2,2,3]
-- conjunto (2 : [2,3]) = 2 : conjunto[y | y <- [2,3], y /= x] = conjunto [3]
-- conjunto (3 : []) = 3 : conjunto[y | y <- [], y /= x] = []
-- conjunto []
-- Resolviendolo queda como:
-- 3 : [] = [3]
-- 2 : [2] = [2,3]
-- 1 : [2,3] = [1,2,3]

numerosPares :: [Int] -> [Int] --Esta función regresa una lista con solo números pares de la lista original.
numerosPares xs = [x | x <- xs, mod x 2 == 0]




