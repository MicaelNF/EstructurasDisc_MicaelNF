data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz a izq der) = 1 + longitud (izq) + longitud (der)

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz a izq der) = 1 + max (profundidad(izq)) (profundidad(der))

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int
ancho ArbolVacio = 0 -- Caso de que desde la consola le pasen un árbol vacío
ancho (Raiz a ArbolVacio ArbolVacio) = 1
ancho (Raiz a izq der) = ancho(izq) + ancho(der)

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio tipo = []
recorrido (Raiz a izq der) InOrder = recorrido(izq) InOrder ++ [a] ++ recorrido(der) InOrder
recorrido (Raiz a izq der) PreOrder = [a] ++ recorrido(izq) PreOrder ++ recorrido(der) PreOrder
recorrido (Raiz a izq der) PosOrder = recorrido(izq) PosOrder ++ recorrido(der) PosOrder ++ [a]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a izq der) = [a] : combinarNiveles (niveles izq) (niveles der)

-- Funcion auxiliar que bueno combina los niveles de dos subárboles.
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] [] = []
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles (xs) (ys)

-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a  
minimo (Raiz a ArbolVacio ArbolVacio) = a
minimo (Raiz a izq der) =
  if a <= minimo(izq) && a <= minimo(der)
    then a
  else if minimo(izq) <= a && minimo(izq) <= minimo(der)
    then minimo(izq)
  else minimo(der)

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a 
maximo (Raiz a ArbolVacio ArbolVacio) = a
maximo (Raiz a izq der) =
  if a >= maximo(izq) && a >= maximo(der)
    then a
  else if maximo(izq) >= a && maximo(izq) >= maximo(der)
    then maximo(izq)
  else maximo(der)

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio elemento = error "No puedes eleminar algo de un árbol vacío" -- Caso que desde la terminal le pasen un ábol vacío.
eliminar (Raiz a ArbolVacio ArbolVacio) elemento =
  if a == elemento
    then ArbolVacio
  else error "El elemento a eliminar no se encuentra dentro del árbol"
  
-- Caso de un nodo con un árbol izquierdo.
eliminar (Raiz a izq ArbolVacio) elemento =
  if elemento == a
    then izq
  else if elemento < a
    then Raiz a (eliminar izq elemento) ArbolVacio
  else error "El elemento a eliminar no se encuentra dentro del árbol"
  
-- Caso de un nodo con un árbol derecho.
eliminar (Raiz a ArbolVacio der) elemento =
  if elemento == a 
    then der
  else if elemento > a
    then Raiz a ArbolVacio (eliminar der elemento)
  else error "El elemento a eliminar no se encuentra dentro del árbol"
  
-- Caso de un nodo que tenga ambos árboles.
eliminar (Raiz a izq der) elemento =
  if elemento == a
    then Raiz (minimo der) izq (eliminar der (minimo der))
  else if elemento > a
    then Raiz a izq (eliminar der elemento)
  else Raiz a (eliminar izq elemento) der
