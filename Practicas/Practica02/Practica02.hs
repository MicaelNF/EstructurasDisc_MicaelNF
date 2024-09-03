longitud :: [x] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud(xs)

sumaLista :: Num x => [x] -> x
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista(xs)

agregaElemento :: [x] -> x -> Bool -> [x]
agregaElemento x xs True  = xs : x
agregaElemento x xs False = x ++ [xs]




