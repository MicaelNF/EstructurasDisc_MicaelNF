longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemento True  = elemento : lista
agregaElemento lista elemento False = lista ++ [elemento]





