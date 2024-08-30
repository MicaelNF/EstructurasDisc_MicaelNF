potenciaLista :: [Int] -> [Int]
potenciaLista [] = []
potenciaLista lista = [x^2 | x <- lista]

agregaElemento :: [x] -> x -> Bool -> [x]
agregaElemento [] x _ = [x]
agregaElemento lista x insertarAlPrincipioOFinal
  | insertarAlPrincipioOFinal = x : lista
  | otherwise = lista ++ [x] 
