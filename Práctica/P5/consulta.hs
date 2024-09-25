

-- 14) 

-- foldN y R para naturales
foldN :: (y -> y)-> y -> Nat -> y
foldN s z 0 = z
foldN s z (n+1) = s (foldN s z n)

pred = foldN .... (puaj)

R :: (y -> Nat -> y) -> y -> Nat -> y
R s z 0 = z
R s z (n+1) = s (R s z n) n 

pred = R (\r n -> n) 0
----------------------------------------------------------

-- foldr y param (similar a R pero para listas) 

foldr :: (x -> y -> y) -> y -> [x] -> y
foldr f e [] = e
foldr f e (x:xs) = f x (foldr f e xs) 

param :: (x -> y -> [x] -> y) -> y -> [x] -> y
param f e [] = e
param f e (x:xs) = f x (param f e xs) xs 

-- insert en Haskell
insert: (a ->  a -> a) -> a -> [a] -> [a]
insert comp x [] = [x]
insert comp x (y:xs) | comp x y = x : y : xs
                     | True  = y : insert comp x xs  

-- intento con foldr
insert x comp = foldr (\y r -> if comp x y then x : y : ? 
                                           else y:r ) [x] 

-- pruebo con param
insert: (a ->  a -> a) -> a -> [a] -> [a]
insert x comp = param (\y r xs -> if comp x y then x : y : xs 
                                              else y:r ) [x]

-- defino insert en Sistema F
insert = \forall X . (X -> X -> Bool) -> X -> List X -> List X
insert = /\ X. \(comp : X -> X -> Bool) -> \(x : X) -> (xs: ListF x) -> 
                    param (List X) (\y: X -> \r: List X -> xs : List X -> 
                                       (comp x y) (cons X x y xs) (cons <X> y r)) -- f 
                                   (cons <X> x (nil <X>)) -- e   


-- defino param como foldr:

foldr :: (x -> (y, [x]) -> (y, [x])) -> (y, [x]) -> [x] -> (y, [x])


param f e ys = fst (foldr (\x (r,xs) -> (f x r xs, x:xs)) (e, []) ys)

-- escribo param en Systema F

 ---------------------------------------------------------------------
-- Práctica 6

data Cont r a = Cont ((a → r ) → r )

(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
Cont g >>= f = Cont (\h -> g (\x -> unCont (f x) h))

          
 g :: (a -> r ) -> r
 f :: a -> Cont r b
 h :: b -> r 


-------------------- Ejemplo de inducción estructural e inducción en la derivación
-- sintaxis
T := if e T T | x := e | skip

-- semántica
<T, s> => s'  

-- Reglas de la semántica
EvalSkip
<skip, s > => s

EvalAssign
<x:=e, s> => s [x -> [[e]]s ]

EvalIfT
[[e]] s = True   y <c1, s> => s'
--------------------------
<if e c1 c2, s> => s'

EvalIfF
[[e]] s = False   y <c2, s> => s'
--------------------------
<if e c1 c2, s> => s'

Quiero probar Propiedad, suponiendo que vale P', para cualquier término t, y estados s ,s1, s'

Propiedad' : [[e]] s = v  /\ s = s1    => [[e]] s1 = v
   
Propiedad : <t, s> => s'  /\ <t, s1> => s1' /\ s = s1    => s' = s1'

Prueba 1)
Inducción en estructural sobre T. 

\forall t . P(t) = \forall s,s',s1,s1' . <t, s> => s'  /\ <t, s1> => s1' /\ s = s1    => s' = s1'

caso T = skip

caso T = x:=e

caso T = if e t1 t2
HI P(t1) y P(t2)
Tesis P(is e t1 t2)
...

Prueba 2)
Inducción en la derivaci´on <T, s> => s'

Hago análisis x casos sobre la última regla de la derivación, 

caso EvalIfF

- [[e]] s = False   
- <c2, s> => s'  (subderivación)
- <if e c1 c2, s> => s'
 
En la derivación <t, s1> => s1', la única regla que puede usar como última regla es 
EvalIfF, porque por Propiedad' concluyo que [[e]] s1 = False. 

- [[e]] s1 = False (por Propiedad')   
- <c2, s1> => s1'  
- <if e c1 c2, s> => s1'

Por hi como <c2, s> => s' , <c2, s1> => s1' y s = s1 concluyo que s' = s1
















 
























 
