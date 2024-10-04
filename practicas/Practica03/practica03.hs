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

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void n elemento =
  if n == 0
    then Node elemento Void  -- Caso base: insertamos en el índice 0 en una lista vacía
  else error "Indice fuera del rango permitido."  -- No podemos insertar en un índice diferente de 0 en una lista vacía
insertarIndice xs 0 elemento = Node elemento xs  -- Insertamos el elemento al principio
insertarIndice (Node x xs) n elemento =
  if n < 0
    then error "Indice fuera del rango permitido."  -- Manejar índices negativos
  else if n == longitud(Node x xs)  -- Permitir inserción al final de la lista
    then Node x (Node elemento xs)  -- Insertar al final
  else if n < longitud(Node x xs)  -- Insertar en un índice válido
    then Node x (insertarIndice xs (n-1) elemento)  -- Recorremos hasta llegar al índice y luego insertamos
  else error "Indice fuera del rango permitido."  -- El índice es mayor que la longitud de la lista

-- Eliminar un elemento en un índice específico
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void n = error "Tu lista esta vacía."
eliminarIndice (Node x xs) n =
  if n == 0
   then xs
  else Node x (eliminarIndice xs (n - 1))

recorrerLista :: List a -> Int -> List a
recorrerLista Void n = Void
recorrerLista xs 0 = xs
recorrerLista (Node x xs) n = recorrerLista (insertarIndice xs (longitud(xs)) x) (n - 1)

--recorrerLista (Node 1 (Node 2 (Node 3 Void))) 1
--Node 1 (Node 2 (Node 3 Void)) 1 = recorrerLista ()
