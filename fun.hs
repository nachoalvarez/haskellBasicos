{- TADs -}
-- Arbol binario
data ArbVacio = Nil
data ArbBin a = Nodo a (ArbBin a) (ArbBin a) | ArbVacio deriving (Show)

-- Arbol n-ario / multiway - rose tree
data ArbonNArio a = NodoN {
        rootLabel :: a,         -- ^ label value
        subBosque :: Bosque a   -- ^ zero or more child trees
    } deriving (Show)
type Bosque a = [ArbonNArio a]

-- | Listar los nodos en cada nivel de un arbol n-ario
nivelesNArio :: ArbonNArio a -> [[a]]
nivelesNArio t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subBosque) [t]

{- aplanarNARio :: ArbolN(a) -> Lista(a) 	-}
aplanarNArio :: ArbonNArio a -> [a]
aplanarNArio t = aplastar t []
  where aplastar (NodoN x ts) xs = x:foldr aplastar xs ts
  -- Sirve para aplanar un árbol N-ario y para recorrerlo en pre-orden

--- ARBOLES N-ARIOS con ubicacion
data ArbolNLoc a  = Loc {
	tree    :: ArbonNArio a,      
	lefts   :: Bosque a,    
	rights  :: Bosque a,    
	parents :: [(Bosque a, a, Bosque a)]
}

arbolNALoc :: ArbonNArio a -> ArbolNLoc a
arbolNALoc t = Loc { tree = t, lefts = [], rights = [], parents = [] }

--fromBosque :: Bosque a -> Maybe (ArbolNLoc a)
--fromBosque (t:ts) = Just Loc { tree = t, lefts = [], rights = ts, parents = [] }
--fromBosque []     = Nothing

--arbolNLocAArbolN :: ArbolNLoc a -> ArbonNArio a
--arbolNLocAArbolN loc = tree (root loc)

--toBosque :: ArbolNLoc a -> ArbonNArio a
--toBosque loc = let r = root loc in combChildren (lefts r) (tree r) (rights r)

insertarNodo :: (Ord a) => a -> ArbBin a -> ArbBin a
insertarNodo x ArbVacio = insertarHoja x
insertarNodo x (Nodo a izq der)
   | x == a = Nodo x izq der
   | x < a  = Nodo a (insertarNodo x izq) der
   | x > a  = Nodo a izq (insertarNodo x der)

insertarHoja :: a -> ArbBin a
insertarHoja x = Nodo x ArbVacio ArbVacio

convertirListaEnArbol :: (Ord a) => [a] -> ArbBin a
convertirListaEnArbol [] = ArbVacio
convertirListaEnArbol (h:t) = convertir2 (Nodo h ArbVacio ArbVacio) t
	where
		convertir2 arbol [] = arbol
		convertir2 arbol (h:t) = convertir2 (insertarNodo h arbol) t

{- existe :: a * ArbBin(a) -> Bool 	-}
{- existe AV = False -}
{- existe (x, AgrArb(x, si, sd) = if (x == a) then true
	else if (x < a) then existe (x, si)
		else (x > a) then existe (x, sd)	-}

existe :: (Ord a) => a -> ArbBin a -> Bool
existe x ArbVacio = False
existe x (Nodo a izq der)  
    | x == a = True
    | x < a  = existe x izq
    | x > a  = existe x der

{-- Template
hacerAlgoSiExiste :: (Ord a) => a -> ArbBin a -> Bool
hacerAlgoSiExiste x ArbVacio = False
hacerAlgoSiExiste x (Nodo a izq der)  
    | x == a = if x == a then procSiExiste a else True
    | x < a  = hacerAlgoSiExiste x izq
    | x > a  = hacerAlgoSiExiste x der
procSiExiste :: a -> a
procSiExiste a = a
--}

rightSubtree :: ArbBin a -> ArbBin a
rightSubtree ArbVacio = ArbVacio
rightSubtree (Nodo _ ArbVacio r) = r
rightSubtree (Nodo v l r) = (Nodo v) (rightSubtree l) r
-- me retorna todos los elementos que esten a la derecha del arbol dado
-- no incluye el más a la izquierda

leftSubtree :: ArbBin a -> ArbBin a
leftSubtree ArbVacio = ArbVacio
leftSubtree (Nodo _ l ArbVacio) = l
leftSubtree (Nodo v l r) = (Nodo v) l $ leftSubtree r
-- me retorna todos los elementos que esten a la izquierda del arbol dado
-- no incluye el más a la derecha

{- subtrees :: ArbBin(a) -> ArbBin(a) 	 -}
subArboles :: ArbBin a -> [ArbBin a]
subArboles ArbVacio = []
subArboles (Nodo x ArbVacio ArbVacio) = []
subArboles (Nodo x izq der) = [Nodo x izq der] ++ subArboles izq ++ subArboles der
-- devuelve todos los subarboles


{- altura :: ArbBin(a) -> Entero 	-}
{- altura (AV) = 0	-}
{- altura ( AgrArb(x, si, sd) = suma (maximo (altura(si), altura(sd)), 1)	-}
altura :: ArbBin a -> Int
altura (ArbVacio) = 0
altura (Nodo _ izq der) = max (altura izq) (altura der) + 1

{- ancestroComun :: a * a * ArbBin(a) -> a 		-}
{- ancestroComun (m, n, AgrArb(x,si,sd)) = if (n < actual) then ancestroComun(m,n,si) 
	else if (m > actual) then ancestroComun (m, n, sd)
		else actual	-}
ancestroComun :: (Ord a) => a -> a -> ArbBin a -> a
ancestroComun m n (Nodo actual izq der)
	| n < actual     = ancestroComun m n izq
	| m > actual     = ancestroComun m n der
	| otherwise = actual

{- minArbol :: ArbBin(a) -> a 	 -}
minArbol :: ArbBin a -> a
minArbol (Nodo x ArbVacio _) = x
minArbol (Nodo x izq _) = minArbol izq

{- maxArbol :: ArbBin(a) -> a 	 -}
maxArbol :: ArbBin a -> a
maxArbol (Nodo x _ ArbVacio) = x
maxArbol (Nodo x _ der) = maxArbol der

{- sumaArbol :: ArbBin(a) -> a 	 -}
sumaArbol :: (Num a) => ArbBin a -> a
sumaArbol ArbVacio = 0
sumaArbol (Nodo x izq der) = x + (sumaArbol izq) + (sumaArbol der)
-- Suma todos los elementos del árbol

{- convertirEnLista :: ArbBin(a) -> Lista(a) 	 -}
convertirEnLista :: ArbBin a -> [a]
convertirEnLista ArbVacio = []
convertirEnLista (Nodo x izq der) = x : a ++ b {- Agl(x , agl(a,b)) -}
  where a = (convertirEnLista izq)
        b = (convertirEnLista der)
-- Convierte un arbol en lista

aplanarArbol :: ArbBin a -> [a]
aplanarArbol ArbVacio = []
aplanarArbol (Nodo a izq der) = aplanarArbol izq ++ [a] ++ aplanarArbol der

{- cantNodos :: ArbBin(a) -> a 	 -}
cantNodos :: (Num a) => ArbBin a -> a
cantNodos ArbVacio = 0
cantNodos (Nodo _ izq der) = cantNodos izq + cantNodos der + 1
-- Cuenta la cantidad de nodos totales de un árbol

{- cantNodos :: ArbBin(a) -> a 	 -}
cantNodosCondicion :: ArbBin Int -> Int
cantNodosCondicion ArbVacio = 0
cantNodosCondicion (Nodo x izq der) = cantNodosCondicion izq + cantNodosCondicion der + ( if x > 8 then 1 else 0)
-- Cuenta la cantidad de nodos que cumplen la condición
-- reemplazar en el parentesis la condicion

{- guardarHojas :: ArbBib(a) -> Lista(a) 	-}
guardarHojas :: ArbBin a -> [a]
guardarHojas ArbVacio = []
guardarHojas (Nodo a ArbVacio ArbVacio) = [a]
guardarHojas (Nodo a izq der) = guardarHojas izq ++ guardarHojas der
-- Guarda las hojas del árbol en una lista

{- guardarNodosCond :: ArbBin(a) -> Lista(a) 	 -}
guardarNodosCond :: ArbBin Int -> [Int]
guardarNodosCond ArbVacio = [] 
guardarNodosCond (Nodo a izq der)
	| a > 8 = [a] ++ guardarNodosCond izq ++ guardarNodosCond der
	| otherwise	= guardarNodosCond izq ++ guardarNodosCond der
-- Guarda los nodos que cumplen la condición en una lista
-- Cambiar condición a > 8 y cambiar el tipo del header por ArbBin a -> [a]

{- nodosEnNivel :: Entero * ArbBin(a) -> Lista(a) 	-}
nodosEnNivel :: Int -> ArbBin a -> [a]
nodosEnNivel _ ArbVacio = []
nodosEnNivel nivel (Nodo valor izq der)
    | nivel == 1 = [valor]
    | nivel > 1  = nodosEnNivel (nivel - 1) izq  ++ nodosEnNivel (nivel - 1) der {- ++ es concat -}
    | otherwise = []

{- preOrden :: ArbBin(a) -> Lista(a) 	-}
preOrden :: ArbBin a -> [a]
preOrden ArbVacio = []
preOrden (Nodo x izq der) = [x] ++ (preOrden izq) ++ (preOrden der)

{- enOrden :: ArbBin(a) -> Lista(a) 	-}
enOrden :: ArbBin a -> [a]
enOrden ArbVacio = []
enOrden (Nodo x izq der) = (enOrden izq) ++ [x] ++ (enOrden der)

{- posOrden :: ArbBin(a) -> Lista(a) 	-}
posOrden :: ArbBin a -> [a]
posOrden ArbVacio = []
posOrden (Nodo x izq der) = (posOrden izq) ++ (posOrden der) ++ [x]

{- TAD Movimiento -}
data Movimiento a = MoverIzquierda a (ArbBin a) | MoverDerecha a (ArbBin a) deriving (Show)
type CaminoRecorrido a = [Movimiento a] {- Camino recorrido es una lista de movimientos realizados -}
type Camino a = (ArbBin a, CaminoRecorrido a) {- Un camino es un árbol y un camino recorrido -}


{- OPERADORES PARA MOVERSE POR EL ARBOL-}
izquierda :: Camino a -> Maybe (Camino a)
izquierda (Nodo x izq der, cs) = Just (izq, MoverIzquierda x der:cs)
izquierda (ArbVacio, _) = Nothing

derecha :: Camino a -> Maybe (Camino a)
derecha (Nodo x izq der, cs) = Just (der, MoverDerecha x izq:cs)
derecha (ArbVacio, _) = Nothing

irArriba :: Camino a -> Maybe (Camino a)
irArriba (t, MoverIzquierda x der:cs) = Just (Nodo x t der, cs)
irArriba (t, MoverDerecha x izq:cs) = Just (Nodo x izq t, cs)
irArriba (_, []) = Nothing

{- LISTAS -}

{- zip :: Lista(a) * Lista(a) -> Lista(a*a) -}
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- zip realiza una combinación / tuplas / parejas con 1 elemento de 1 lista y 1 de la otra

{- sumarLista :: Lista(a) -> Entero 	 -}
--sumarLista :: [a] -> Int
--sumarLista [] = 0
--sumarLista (x:xs) = x + sumarLista xs

{- inversa Lista(a) -> Lista(a) -}
-- inversa LV = LV
-- inversa agl(x, xs) = concat(inversa xs, x)
inversa :: [a] -> [a]
inversa []     = []
inversa (x:xs) = inversa xs ++ [x]
-- inversa [1,2,3,4,5] -> [5,4,3,2,1]

{- longitudLista :: Lista(a) -> Entero 	-}
longitudLista :: [a] -> Integer
longitudLista [] = 0
longitudLista (_:xs) = 1 + longitudLista xs


{- maxLista :: Lista(a) -> a -}
-- maxLista LV = error "..."
-- maxLista [x] = x
-- maxLista (agl(x, xs)) = max (x, maxLista xs)
maxLista :: (Ord a) => [a] -> a
maxLista [] = error "lista vacia"
maxLista [x] = x
maxLista (x:xs) = max x (maxLista xs)
-- maxLista [5,10,2,49] -> 49

{- existeLista :: a * Lista(a) -> Bool -}
-- existeLista ( _ , LV) = False
-- existeLista (a, agl(x, xs)) = if a == x then True else existeLista(a, xs)
existeLista :: (Eq a) => a -> [a] -> Bool
existeLista _ [] = False
existeLista a (x:xs)
    | a == x    = True
    | otherwise = existeLista a xs

{- quicksort :: Lista(a) -> Lista(a) -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
        let masChicosOIguales = [a | a <- xs, a <= x]
            masGrandes        = [a | a <- xs, a > x]
        in quicksort masChicosOIguales ++ [x] ++ quicksort masGrandes
-- quicksort [1,29,10,23,35,92,22,11] -> [1,10,11,22,23,29,35,92]

{- replicar :: Entero -> Lista(a) -}
replicar :: Int -> a -> [a]
replicar n x
    | n <= 0    = []
    | otherwise = x : replicar (n-1) x
-- replicar 4 'n' -> "nnnn"

{- agarrar :: Entero * Lista(a) -> Lista(a) -}
agarrar :: Int -> [a] -> [a]
agarrar n _
    | n <= 0   = []
agarrar _ []     = []
agarrar n (x:xs) = x : agarrar (n-1) xs
-- agarrar 4 [1,2,3,4,5,6] -> [1,2,3,4]

{- dividirListaEnPosN :: Lista(a) * Entero -> Lista(a)*Lista(a)	-}
dividirListaEnPosN :: [a] -> Int -> ([a],[a])
dividirListaEnPosN [] _ = ([], [])
dividirListaEnPosN l@(x:xs) n
  | n > 0 = (x:ys, zs)
  | otherwise = ([], l)
  where (ys, zs) = dividirListaEnPosN xs (n-1)
-- divide una lista en 2 listas a la posición N dada


{-String en lista-}
cadenaAListaEnPosN :: String -> Int -> (String,String)
cadenaAListaEnPosN [] _ = ([], [])
cadenaAListaEnPosN l@(x:xs) n
  | n > 0 = (x:ys, zs)
  | otherwise = ([], l)
  where (ys, zs) = cadenaAListaEnPosN xs (n-1)
-- divide una cadena en 2 cadenas/listas

{- sliceLista :: Lista(a) * Entero * Entero -> Lista(a) -}
sliceLista ::[a] -> Int -> Int -> [a]
sliceLista [] _ _ = []
sliceLista (x:xs) d h
  | d > 1 = sliceLista xs (d-1) (h-1)
  | h < 1 = []
  | otherwise = x:sliceLista xs (d-1) (h-1)
-- Dado 2 enteros, d h (desde-hasta), devuelve una lista con los elementos entre ambos

{- moverAlfinalN :: Lista(a) * Entero -> Lista(a) 	-}
moverAlFinalN ::[a] -> Int -> [a]
moverAlFinalN [] _ = []
moverAlFinalN xs n = drop len xs ++ take len xs
  where len = n `mod` length xs
-- Mueve los primeros N items al final de la lista

separarItemLista :: Int -> [a] -> (a, [a])
separarItemLista 1 (x:xs) = (x, xs)
separarItemLista n (x:xs) = (l, x:r)
  where (l, r) = separarItemLista (n-1) xs
-- Dada una posición separa ese elemento de la lista y devuelve el elemento y el resto de la lista

eliminarChar :: Int -> [Char] -> (Char, [Char])
eliminarChar 1 (x:xs) = (x, xs)
eliminarChar n (x:xs) = (l, x:r)
  where (l, r) = eliminarChar (n-1) xs
-- Dada una posición N separa el char y devuelve una tupla del char y resto de la cadena

{- insertarEnListaPosN :: Entero * Lista(a) * Entero -> Lista(a) -}
insertarEnListaPosN :: a -> [a] -> Int -> [a]
insertarEnListaPosN c xs n = ls ++ [c] ++ rs
  where ls = take (n-1) xs
        rs = drop (n-1) xs
-- Dado un elemento, una lista y una posición N; añade el elemento en la posición dada

{- insertarChar :: Char * Lista(char) * Entero -> Lista(char) -}
insertarChar :: Char -> [Char] -> Int -> [Char]
insertarChar c xs n = ls ++ [c] ++ rs
  where ls = take (n-1) xs
        rs = drop (n-1) xs
-- Similar anterior pero con Char

{-juntarDuplicadosConsecutivos :: Lista(a) -> Lista(Lista(a)) 	 -}
juntarDuplicadosConsecutivos :: (Eq a) => [a] -> [[a]]
juntarDuplicadosConsecutivos x = foldl (\a b -> if take 1 (last a) == [b] then (init a) ++ [last a ++ [b]] else a ++ [[b]]) [[head x]] (tail x)
-- Crear una lista que contenga todos los duplicados consecutivos
-- ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
-- similar a Group

{- eliminarDuplicadosChar :: Lista(a) -> Lista(a) 	-}
eliminarDuplicadosConsecutivos :: (Eq a) => [a] -> [a]
eliminarDuplicadosConsecutivos x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x
-- Elimina duplicados consecutivos de una lista, devuelve una lista sin consecutivos repetidos

{- eliminarDuplicadosChar :: Lista(Char) -> Lista(Char) 	-}
eliminarDuplicadosChar :: [Char] -> [Char]
eliminarDuplicadosChar x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x
-- Elimina duplicados consecutivos "aaaabccaadeeee" > en "abcade"

{- anteUltimoLista :: Lista(a) -> a	 -}
anteUltimoLista :: [a] -> a
anteUltimoLista (x:[_]) = x
anteUltimoLista (_:xs) = anteUltimoLista xs

{- ultimoLista :: Lista(a) -> a	 -}
ultimoLista :: [a] -> a
ultimoLista (x:[]) = x
ultimoLista (_:xs) = ultimoLista xs

{- posNLista :: Entero * Lista(a) -}
posNLista :: Int -> [a] -> a
posNLista n list = list !! (n-1)
-- devuelve el elemento N de una lista

{- unirListas :: Lista(a)* Lista(a) -> Lista(a)	-}
-- unirListas (n, m) = concat(n, m)
unirListas :: [a] -> [a] -> [a]
unirListas n m = n ++ m

{- interseccionLista :: Lista(a) * Lista(a) -> Lista(a)	-}
interseccionLista :: (Eq a) => [a] -> [a] -> [a]
interseccionLista =  interseccionPor (==)

interseccionPor :: (a -> a -> Bool) -> [a] -> [a] -> [a]
interseccionPor _  [] _     =  []
interseccionPor _  _  []    =  []
interseccionPor eq xs ys    =  [x | x <- xs, any (eq x) ys]
-- interseccion [1,2,2,3,4] [6,4,4,2] 	> [2,2,4]

{- precederATodos :: a * Lista(a) -> Lista(a) -}
precederATodos :: a -> [a] -> [a]
precederATodos _   []     = []
precederATodos sep (x:xs) = sep : x : precederATodos sep xs
-- precederATodos 10 [2,3,4,5] > [10,2,10,3,10,4,10,5]

{- entremezclarLista :: a * Lista(a) -> Lista(a) 	-}
-- es intersperse
entremezclarLista :: a -> [a] -> [a]
entremezclarLista _  [] = []
entremezclarLista sep (x:xs)  = x : precederATodos sep xs
-- entremezclarLista 1 [2,3,4,5] > [2,1,3,1,4,1,5]
-- agregar mi dato luego de cada elemento de la lista, menos al final

{- intercalarLista :: Lista(a) * Lista(Lista(a)) -> Lista(a) 	 -}
intercalarLista :: [a] -> [[a]] -> [a]
intercalarLista xs xss = concat (entremezclarLista xs xss)
-- intercalarLista ", " ["Nombre 1", "Nombre 2", "Nombre 3"] > "Nombre 1, Nombre 2, Nombre 3"
-- sirve para dar meter los elementos de una lista dentro de un formato

{- transponerLista :: Lista(Lista(a)) -> Lista(Lista(a)) 	-}
transponerLista :: [[a]] -> [[a]]
transponerLista [] = []
transponerLista ([]: xss) = transponerLista xss
transponerLista ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transponerLista (xs : [ t | (_:t) <- xss])
-- Forma tuplas-parejas entre de 1:1 elementos de cada nivel de sublistas
-- transponerLista [[1,2,3,4,5,6],[9,10,11],[20,30,40]] > [[1,9,20],[2,10,30],[3,11,40],[4],[5],[6]]
-- Los que sobran sin pareja quedan solos

{- noVaciasSubsecuencias :: Lista(a) -> Lista(Lista(a))	 -}
noVaciasSubsecuencias :: [a] -> [[a]]
noVaciasSubsecuencias [] =  []
noVaciasSubsecuencias (x:xs) =  [x] : foldr f [] (noVaciasSubsecuencias xs)
  where f ys r = ys : (x : ys) : r
-- es similar a subsequences pero no retorna la lista vacia
-- noVaciasSubsecuencias "abc" > ["a","b","ab","c","ac","bc","abc"]

{- todosConTodosLista :: Lista(a) -> Lista(Lista(a))	 -}
todosConTodosLista :: [a] -> [[a]]
todosConTodosLista xs =  [] : noVaciasSubsecuencias xs
-- similar a subsequences
-- todosConTodosLista [1,2,3] > [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

{- permutaciones :: Lista(a) -> Lista(Lista(a)) 	-}
permutaciones :: [a] -> [[a]]
permutaciones xs0 =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr entrelazado (perms ts (t:is)) (permutaciones is)
      where entrelazado    xs     r = let (_,zs) = entrelazado' id xs r in zs
            entrelazado' _ []     r = (ts, r)
            entrelazado' f (y:ys) r = let (us,zs) = entrelazado' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-- Es la cantidad de combinaciones posibles con todos los elementos de la lista
-- permutaciones [1,2,3] > [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

{- tomarMientras :: (a*Bool) * Lista(a) -> Lista(a) 	-}
tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] =  []
tomarMientras p (x:xs)
            | p x  =  x : tomarMientras p xs
            | otherwise =  []
-- Solo me devuelve la parte de la lista Mientras que cumple con mi condición, cuando deja de cumplir corta
-- Ej: tomarMientras (>3) [6,5,4,1,2,5,4,1] 	> [6,5,4]
-- Lo puedo concatenar con otras, por ej sumar solo los que cumplen la condición:
-- sum $ tomarMientras (>3) [6,5,4,1,2,5,1] 	> 15

{- eliminarMientras :: (a*Bool) * Lista(a) -> Lista(a) -}
eliminarMientras :: (a -> Bool) -> [a] -> [a]
eliminarMientras _ []  =  []
eliminarMientras p xs@(x:xs')
            | p x  =  eliminarMientras p xs'
            | otherwise =  xs
-- Tira la parte de la lista hasta que se cumple la condición y devuelve el resto
-- Ej: eliminarMientras (<3) [1,2,2,2,3,4,5,4,3,2,1] 	> [3,4,5,4,3,2,1]

{- partirListaEnCondicion :: (a*Bool) * Lista(a) -> Lista(a) 	-}
partirListaEnCondicion :: (a -> Bool) -> [a] -> ([a],[a])
partirListaEnCondicion _ xs@[] =  (xs, xs)
partirListaEnCondicion p xs@(x:xs')
           | p x  =  ([],xs)
           | otherwise  =  let (ys,zs) = partirListaEnCondicion p xs' in (x:ys,zs)
-- es la función break
-- divide la lista cuando el predicado se cumple por primera vez
-- Ej: partirListaEnCondicion (==3) [1,2,3,4,5,6,7] 		> ([1,2],[3,4,5,6,7])

{- particionLista :: (a*Bool) * Lista(a) -> Lista(Lista(a), Lista(a)) 	-}
particionLista :: (a -> Bool) -> [a] -> ([a],[a])
particionLista p xs = foldr (selectLista p) ([],[]) xs

selectLista :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
selectLista p x ~(ts,fs) 	| p x  = (x:ts,fs)
                    		| otherwise = (ts, x:fs)
-- es partition
-- particionLista (>3) [1,5,2,4,3] 		> ([5,4],[1,2,3])
-- Dada una lista y una condición, me devuelve 2 listas, una con los que cumplen y otra con los que no2

{- filtrarLista :: (a*Bool) * Lista(a) -> Lista(a)	-}
filtrarLista :: (a -> Bool) -> [a] -> [a]
filtrarLista _pred []    = []
filtrarLista pred (x:xs)
  | pred x         = x : filtrarLista pred xs
  | otherwise      = filtrarLista pred xs
-- MUY IMPORTANTE, es un filter
-- devuelve una lista solo con los elementos que cumplen con la condicion
-- Ej: filtrarLista (>8) [1,2,5,8,9,1,20,10] 	> [9,20,10]


{- buscarPrimeroQueCumpleCondicion :: (a*Bool) * Lista(a) -> a  -}
buscarPrimeroQueCumpleCondicion :: (a -> Bool) -> [a] -> a
buscarPrimeroQueCumpleCondicion p xs = head (filtrarLista p xs)
-- buscarPrimeroQueCumpleCondicion (>3) [0,2,4,6,8] 	> 4
-- es similar a find

-- buscarDiccionario 'c' [('a',0),('b',1),('c',2)]
-- contar o aplicar función sobre los elementos de un diccionario o lista por clave
-- filter ((==1).fst) [(1,2), (3,4), (1,8)]  > 	[(1,2),(1,8)]		--> funciona

tuplaALista :: [(a,a)] -> [a]
tuplaALista ((a,b):xs) = a : b : tuplaALista xs
tuplaALista _ = []


-- TAD ListaAnidada
data ListaAnidada a = Elem a | List [ListaAnidada a]
aplanarLista :: ListaAnidada a -> [a]
aplanarLista (Elem a) = [a]
aplanarLista (List []) = []
aplanarLista (List (x:xs)) = aplanarLista x ++ aplanarLista (List xs)

-- PLIEGUES
-- foldr
-- pliegueDer :: (a -> b -> b) -> b -> [a] -> b
-- pliegueDer k z = go
--          where
--            go []     = z
--            go (y:ys) = y `k` go ys

-- foldl
-- pliegueIzq :: (a -> a -> a) -> [a] -> a
-- pliegueIzq f (x:xs) =  pliegueIzq f x xs
-- pliegueIzq _ [] =  error "..."


{- MAP -}
-- map :: (a->b) -> [a] -> [b]
-- map f [] =  []
-- map f (x:xs) = f x : map f xs
-- Toma una función y la aplica a todos los elementos de la lista
-- Ej: map (>4) [5,6,7,2] > [True,True,True,False]
-- Ej: map (+3) [1,5,3,1,6]  >   ([4,8,6,4,9])

-- map para aplicar sobre listas: saca los elementos de la lista, los concatena aplicando lo que le diga
-- concatMap  :: (a -> [b]) -> [a] -> [b]
-- concatMap f =  foldr ((++) . f) []
-- Ej: concatMap (++"! ") ["uno", "dos", "tres"] > "uno! dos! tres!" 

-- LAMBDAS
-- \a b -> (a * 30 + 3)/b
-- Ej: map (\(a,b) -> a + b) [(1,2),(3,5)]

{- GENERALES -}
-- otherwise :: Bool
-- otherwise =  True

-- AND en listas
-- Devuelve True si todos son verdaderos
-- Ej: and $ map (>4) [5,6,7,8] > True
-- Ej: and $ map (==4) [4,4,4,3,4] > False

-- OR en listas
-- Devuelve True si alguno es verdadero
-- Ej: or $ map (==4) [2,3,4,5,6,1] > True
-- Ej: or $ map (>4) [1,2,3] > False

-- Eliminar repetidos de una lista, con nub
-- nub :: (Eq a) => [a] -> [a]
-- nub =  nubBy (==)
-- nubBy :: (a -> a -> Bool) -> [a] -> [a]
-- nubBy eq [] =  []
-- nubBy eq (x:xs) =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)

-- TUPLAS
-- primer elemento tupla
-- fst :: (a,b) -> a
-- fst (x,_) =  x

-- segundo elemento tupla
-- snd :: (a,b) -> b
-- snd (_,y) =  y

-- Def para una tupla de más elementos
-- hay que ajustar este patron y también el de la funcion tuplaAlistas
primt5 :: (a,b,c,d,e) -> a
primt5 (x,_,_,_,_) =  x

segt5 :: (a,b,c,d,e) -> a
segt5 (_,x,_,_,_) =  x

-- algo de GRAFOS
type Vertice = Int
type Arista = (Vertice, Vertice)
type Grafo = [Arista]

grafo :: [Arista]
grafo = [
		(1,2), (1,3), (1,4), (1,5),
		(2,6), (2,7), (4,8), (5,9)
	]

adyacentesG :: Grafo -> Vertice -> [Vertice]
adyacentesG [] _ = []
adyacentesG ((a,b):c) v
		| (a == v) = b:(adyacentesG c v)
		| (b == v) = a:(adyacentesG c v)
		| otherwise = adyacentesG c v
