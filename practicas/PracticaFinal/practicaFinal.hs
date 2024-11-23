data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom elemento) = [elemento] 
variables (Neg formula) = conjunto(variables formula)
variables (formula1 :&: formula2) = conjunto(variables formula1 ++ variables formula2)
variables (formula1 :|: formula2) = conjunto(variables formula1 ++ variables formula2)
variables (formula1 :=>: formula2) = conjunto(variables formula1 ++ variables formula2)
variables (formula1 :<=>: formula2) = conjunto(variables formula1 ++ variables formula2)

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x : xs) = x : conjunto[y | y <- xs, y /= x]
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom elemento) = Neg (Atom elemento)
negacion (Neg formula) = formula
negacion (formula1 :&: formula2) = negacion formula1 :|: negacion formula2
negacion (formula1 :|: formula2) = negacion formula1 :&: negacion formula2
negacion (formula1 :=>: formula2) = formula1 :&: negacion formula2
negacion (formula1 :<=>: formula2) = (formula1 :&: negacion formula2) :|: (formula2 :&: negacion formula1)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom elemento) = Atom elemento
equivalencia (Neg formula) = negacion formula
equivalencia (formula1 :&: formula2) = equivalencia formula1 :&: equivalencia formula2
equivalencia (formula1 :|: formula2) = equivalencia formula1 :|: equivalencia formula2
equivalencia (formula1 :=>: formula2) = negacion (formula1 :&: negacion formula2)
equivalencia (formula1 :<=>: formula2) = negacion ((formula1 :&: negacion formula2) :|: (formula2 :&: negacion formula1))
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion formula valores = if sonIguales (variables formula) (extraerVariables valores)
                                    then evalua (equivalencia formula) valores
                                    else error "No todas las variables están definidas."

evalua :: Formula -> [(Var, Bool)] -> Bool
evalua (Atom elemento) valores = buscaValor elemento valores
evalua (Neg f) valores = not (evalua f valores)
evalua (f1 :&: f2) valores = evalua f1 valores && evalua f2 valores
evalua (f1 :|: f2) valores = evalua f1 valores || evalua f2 valores

buscaValor :: Var -> [(Var, Bool)] -> Bool
buscaValor var ((valorLista, booleano) : xs) = if var == valorLista
                                                then booleano
                                                else buscaValor var xs

sonIguales :: [Var] -> [Var] -> Bool
sonIguales [] [] = True
sonIguales lista [] = False
sonIguales [] lista = False
sonIguales (x : xs) (y : ys) = estaContenido y (x : xs) && sonIguales xs ys

estaContenido :: Eq a => a -> [a] -> Bool
estaContenido elemento [] = False
estaContenido elemento (x : xs) = elemento == x || estaContenido elemento xs

extraerVariables :: [(Var, Bool)] -> [Var]
extraerVariables [] = []
extraerVariables ((elemento, booleano) : xs) = elemento : extraerVariables xs
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones formula = combinarListas (variables formula) (numeroDeCiclos [[False], [True]] ((contador (variables formula)) - 1))

combinacionesDeLosBooleanos :: [[Bool]] -> Bool -> [[Bool]]
combinacionesDeLosBooleanos [] booleano = []
combinacionesDeLosBooleanos ((y : ys) : xs) booleano = ((y : ys) ++ [booleano]) : (((y : ys) ++ [not booleano]) : combinacionesDeLosBooleanos xs booleano)

numeroDeCiclos :: [[Bool]] -> Int -> [[Bool]]
numeroDeCiclos lista 0 = lista
numeroDeCiclos lista numero = numeroDeCiclos (combinacionesDeLosBooleanos lista False) (numero - 1)

combinarListas :: [Var] -> [[Bool]] -> [[(Var, Bool)]]
combinarListas [] [] = []
combinarListas lista [] = []
combinarListas lista (y : ys) = combinarElementos lista y : combinarListas lista ys

combinarElementos :: [Var] -> [Bool] -> [(Var, Bool)]
combinarElementos [] [] = []
combinarElementos lista [] = []
combinarElementos (x : xs) (y : ys) = (x,y) : combinarElementos xs ys

contador :: [Var] -> Int
contador [] = 0
contador (x : xs) = 1 + contador xs
---------------------------------------------------

-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad formula = evaluarMultiplesElementos formula (combinaciones formula)

evaluarMultiplesElementos :: Formula -> [[(Var,Bool)]] -> [([(Var,Bool)],Bool)]
evaluarMultiplesElementos formula [] = []
evaluarMultiplesElementos formula (x : xs) = (x , interpretacion formula x) : evaluarMultiplesElementos formula xs
-----------------------------------------------------