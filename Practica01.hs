distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt (((x1 - x2)^2) + ((y1 - y2)^2))

hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt ((b^2) + (h^2))

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = ((-b + sqrt ((b^2) - (4 * a * c))) / (2 * a) , (-b - sqrt ((b^2) - (4 * a * c))) / (2 * a))

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt ((( (a + b + c) / 2 ) * (( (a + b + c) / 2 ) - a) * (( (a + b + c) / 2 ) - b) * (( (a + b + c) / 2 ) - c)))

comparador :: Int -> Int -> Int
comparador x y = if x == y
    then 0
    else if x < y
      then (-1)
      else 1

maximo :: Int -> Int -> Int -> Int
maximo x y z = if x > y
    then if x > z
      then x
      else z
    else if y > z
      then y
      else z

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x > y
    then if y > z
      then if z > w
        then True
        else False
      else False
    else False
