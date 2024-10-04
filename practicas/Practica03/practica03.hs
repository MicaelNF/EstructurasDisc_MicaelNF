data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node x xs) = 1 + longitud xs

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void n = False
estaContenido (Node x xs) y =
  if x == y
    then True
  else estaContenido xs y

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x : xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x xs) = x : convertirALista xs

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) =
  if estaContenido xs x
   then conjunto xs
  else Node x (conjunto xs)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void n = error "Tu lista esta vacía."
eliminarIndice (Node x xs) 0 = xs
eliminarIndice (Node x xs) n =
  if n < 0 
    then error "Indice fuera del rango permitido."
  else if n < longitud(Node x xs)
    then Node x (eliminarIndice xs (n-1))
  else error "Indice fuera del rango permitido."

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void n elemento = Node elemento Void
insertarIndice xs 0 elemento = Node elemento xs
insertarIndice (Node x xs) n elemento =
  if n < 0 
    then error "Indice fuera del rango permitido."
  else if n <= longitud(Node x xs)
    then Node x (insertarIndice xs (n-1) elemento)
  else error "Indice fuera del rango permitido."
  
recorrerLista :: List a -> Int -> List a
recorrerLista Void n = Void
recorrerLista xs 0 = xs
recorrerLista (Node x xs) n = recorrerLista (insertarIndice xs (longitud xs) x) (n - 1)
