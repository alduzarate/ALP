-- EJERCICIO 14

data BinTree a = Leaf | Bin a (BinTree a) (BinTree a) deriving Show

foldBin :: BinTree a -> b -> (a -> b -> b -> b) -> b 
foldBin Leaf l b = l
foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)


-- a) Definimos las funciones en Haskell

-- i
isLeaf :: BinTree a -> Bool 
isLeaf t = foldBin t True (\ _ _ _ -> False)

-- ii
mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f t = foldBin t Leaf (Bin . f)

-- iii
heightBin :: BinTree a -> Int 
heightBin t = foldBin t 0 (\ _ x y -> max x y + 1)

-- iv
mirrorBin :: BinTree a -> BinTree a
mirrorBin t = foldBin t Leaf (\ a l r -> Bin a r l)


-- b)

{- 

Basándonos en la definición de foldBin que dimos en haskell, 
pensemos cómo podríamos "traducirlas" a lambda cálculo

foldBin Leaf l b = l
foldBin Leaf = \ l b -> l

foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)  
foldBin (Bin a t u) = \ l b -> b a (foldBin t l b) (foldBin u l b)

Entonces pensemos que si definimos foldBin como:
def foldBin = \ x . x

Luego queda que:
def leaf = \ l b . l
def bin = \ raiz left right . (\ l b . b raiz (left l b) (right l b))

Y esta es la representación que usaremos.
-}


-- c)

{-
Para definir ahora las funciones en lambda-cálculo, lo que haremos será
simplemente basarnos en la definición que dimos en Haskell, teniendo en
cuenta que tenemos nuestro foldBin definido como la función identidad, 
por lo que simplemente debemos armar funciones que tomen un árbol y le
apliquen a este el caso base y el paso recursivo.

Definimos las dos primeras funciones:

def isLeaf = \ t . t true (\ x y z . false)

def mapBin = \ f t . t leaf (\ x . bin (f x))

Para el caso de heightBin vemos que necesitamos la función max, vamos a 
armarla valiéndonos de distintas funciones.

def is0 = \ n . n (\x . false) true
def pred = \ n . fst (n (\p . pair (snd p) (suc (snd p))) (pair zero zero))
def resta = \ n m . m pred n
def lt = \ n m . is0 resta n m
def max = \ x y . (lt x y) y x

Ahora sí,

def heightBin = \ t . t 0 (\ z x y . suc (max x y))

Y, por último,

def mirrorBin = \ t . t leaf (\ a l r . bin a r l)

Ahora bien, el ejercicio nos pide que verifiquemos que para algún árbol no 
trivial las aplicaciones de las funciones a este sean beta-equivalentes al
resultado esperado.

Para ello, podrían trabajar con un arbol así 

def arbol = bin 0 (bin 0 leaf leaf) leaf 

y hacer las reducciones en papel. Una vez hecho eso, si quieren verificar
o jugar con otros ejemplos pueden entrar a la página lambdacalc.io donde 
dado un término pueden ver sus beta-reducciones y pueden definirse términos 
de manera cómoda en la barra lateral para no perderse con tanto reemplazo 
(simplemente escriben la igualdad -sin def- y como lambda usen \ como hicimos acá)
-}