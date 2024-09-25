{-# LANGUAGE TypeApplications #-}  

{- Permite aplicaciones de tipo visibles en expresiones. Ej read @Int "4" -}

{-# LANGUAGE ImpredicativeTypes #-}

{- Permite instancias de funciones polimórficas cuyo tipo no sea monomórfico -} 

{-# LANGUAGE ScopedTypeVariables #-}

{- Para que las variables del tipo polimórfico estén al alcance de la función -}

-- Ej 14 - práctica 4)

-- a)

-----------------------
-- en Haskell  
------------------------
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> f x : ys) [] xs 

type List x = forall y. (x -> y -> y) -> y -> y

---------------------------------------
-- En Sistema F
---------------------------------------

-- versión 1

{- map = Λ x y . λf:(x -> y) . λxs: List x . 
           xs ⟨List y⟩ (λhd:x tl:List y. cons <x> (f hd) tl) (nil <x>)   
-}

-- Codificado en Haskell con las extensiones 

nil :: forall x . List x
nil = \(c :: x -> y -> y) (n :: y) -> n

cons :: forall x . x -> List x -> List x
cons = \(v :: x) (xs:: List x) -> \(c :: x -> y -> y) (n :: y) -> c v (xs @y c n) 

map1 = \(f :: x->y) (xs :: List x) -> 
            xs @(List y) (\(hd :: x) (tl :: List y) -> cons @y (f hd) tl) (nil @y) 


-- versión 2

{- map = Λ x y . λf:(x -> y) . λxs: List x . 
           Λ w . λc: y -> w -> w . λn:w .
           xs ⟨w⟩ (λhd:x tl:w. c (f hd) tl) n   
-}

-- Codificado en Haskell con las extensiones 
map2 :: forall x . forall y . (x->y)-> List x -> List y
map2 = \(f :: x->y) (xs :: List x) (c :: y -> w -> w) (n :: w) -> 
            xs @w (\(hd :: x) (tl :: w) -> c (f hd) tl) n 


-- b) 
-----------------------
-- en Haskell  
------------------------

insert :: (a -> a -> Bool) -> a -> [a] -> [a]
insert comp x [] = [x]
insert comp x (y :xs) | comp x y = x : y : xs  -- este caso no lo puedo escribir con foldr
                      | True     = y : insert comp x xs 
             
-- Vemos que necesitamos la defición de paramorfismo
-- similar a foldr pero la función del primer argumento
-- tiene un argumento extra (la lista sin consumir)
                    
param :: (a -> b -> [a] -> b) -> b -> [a] -> b
param c n [] = n
param c n (x:xs) = c x (param c n xs) xs                    

-- Con ésta función podemos escribir insert

insert' :: (a -> a -> Bool) -> a -> [a] -> [a]
insert' comp x ys = param (\y b xs -> if comp x y then x:y:xs else y:b) [x] ys
 
-- Entonces para escribir insert en Sistema F
-- definimos param en término de foldr 
           -- (x -> (y,[x]) -> (y,[x])) -> (y.[x]) -> [x] -> (y,[x])
param' c n xs = fst (foldr (\x (y,ys) -> (c x y ys, x:ys)) (n,[]) xs)

-----------------------
-- en Systema F  
------------------------
{-
-- traducción de fst (foldr (\x (y,ys) -> (c x y ys, x:ys)) (n,[]) xs)
param : forall a . forall b . (a -> b -> List a -> b) -> b -> List a -> b
param = Λ X . Λ Y . λ c: X -> Y -> List X -> Y . λ n: Y . λ xs: List X . 
  fst <Y> <List X> (xs <Pair Y (List X)>  
                       (λx: X . λp: Pair Y (List X). 
                           pair <Y> <List X> (c x (fst <Y> <List X> p)
                                                  (snd <Y> <List X> p)) 
                                                  (cons <X> x : snd <Y> <List X> p)) 
                       (pair <Y> <List X> n (nil <X>)))          

-}
{-
-- traducción de insert or x ys = param (\y b xs -> if or x y then x:y:xs else y:b) [x] ys 

insert : forall a. (a -> a -> BoolF) -> a -> List a -> List a
insert = Λ X. λ comp: X -> X -> BoolF. λx :X .λxs: List X . 
                 param <X> <List X> (λy:X .λys: List X . λzs: List X . 
                                         (comp x y) 
                                             (cons <X> x (cons <X> y ys)) 
                                             (cons <X> y ys)) 
                                    (cons <X> x (nil <X>)) xs  

-}





















































