--------------- Listas y recursión ---------------

longitud :: [a] -> Int --Muestra la cantidad de elementos que hay en una lista.
longitud [] = 0
longitud (x : xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a --Suma todos los elementos de una lista de tipo numérico.
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a] --Agrega un elemento a una lista ya sea al inicio o al final dependiendo del valor booleano.
agregaElemento lista elemento True  = elemento : lista
agregaElemento lista elemento False = lista ++ [elemento]

maximoLista :: (Num a, Ord a) => [a] -> a --Calcula el valor máximo de una lista
maximoLista [] = 0
maximoLista (x : xs) =
  if x > maximoLista(xs)
    then x
    else maximoLista(xs)

indice :: [a] -> Int -> a --Busca la posicion en la que se encuntra cierto elemento en una lista a partir de un numero entero dado.
indice (x : _) 0 = x 
indice (_ : xs) numero = indice(xs) (numero-1)

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int] --Esta función calcula y genera una lista de los números que pueden dividir a cierto numero dado .
divisores 0 = []
divisores xs = [x | x <- auxiliarDeDivisores(xs), mod xs x == 0]

auxiliarDeDivisores :: Int -> [Int] --Esta es una funcion que genera una lista que necesita la funcion "divisores".
auxiliarDeDivisores x =
  if x < 1
    then []
    else x : auxiliarDeDivisores (x - 1)

conjunto :: Eq a => [a] -> [a] --Esta funcion genera una lista sin elementos repetidos.
conjunto [] = []
conjunto (x : xs) = x : conjunto[y | y <- xs, y /= x]

numerosPares :: Num a => [a] -> [a]
numerosPares [] = []
numerosPares xs = [x | x <- xs]



