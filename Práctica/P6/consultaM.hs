---------------------------
--Propiedad de let para tp
--------------------------
{-  Si y ∉ FV (g x) entonces: -} 

  let x = let y = f
          in h y 
  in g x  
   
   = 
   
   let y = f 
       in let x = h y
          in g x 
          
 
{-          

data St a s = State (s -> (a,s))
          
 x :: St (s -> (a,s))         

Análisis x casos sobre x, donde tenemos un sólo caso 

x = State f    para algún f       

-}       
       
       
 -- 5) 
 
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    
-- b)           
newtype Output w a = Out (a, w )   
       
instance Monoid m => Monad (Output w) where
	return x = Out (x, mempty)        
	Out (x,w) >>= f = let Out (y,w')  = f x 
	                  in Out (y, mappend w w')
	                 

-- c)
{-
Prueba de la tercera ley de Monad: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-}	
m = Out (x,w)

(Out (x,w) >>= f) >>= g

= { >>=.1 }  

(let Out (y,w')  = f x 
 in Out (y, mappend w w')) >>= g

 = {Propiedad de let: g (let x = e in y) = let x= e in g y } 

 let Out (y,w')  = f x 
 in Out (y, mappend w w') >>= g

 = { >>=.1 }

 let Out (y,w')  = f x 
 in let (z, w'') = g y 
    in Out (z, mappend (mappend w w') w'')

Por otro lado, 
 
 Out (x,w) >>=(\y ->  f y >>= g)

 = { >>=.1 }

 let Out (z, o)  =  (\y ->  f y >>= g) x 
 in Out (z, mappend w o)

 = {applicación}

 let Out (z, o)  =  f x >>= g 
 in Out (z, mappend w o)

 = {agrego def. local, f x = Out (y,w') para algún 'y' y 'w''}

 let Out (y,w') = f x
 in let Out (z, o)  =  f x >>= g 
    in Out (z, mappend w o)
}
 = {del. local}

let Out (y,w') = f x
 in let Out (z, o)  =  Out (y,w') >>= g 
    in Out (z, mappend w o)

 = { >>=.1 }

let Out (y,w') = f x
 in let Out (z, o)  =  let Out (r, s) = g y 
                       in Out (r,mappend w' s) 
    in Out (z, mappend w o)

 = { Prop let }

let Out (y,w') = f x
in let Out (r, s) = g y
   in let Out (z, o)  = Out (r,mappend w' s) 
       in Out (z, mappend w o)

= { def local }
  
let Out (y,w') = f x
in let Out (r, s) = g y
   in Out (r, mappend w (mappend w' s))
   
= { mappend es asoc. } 

let Out (y,w') = f x
in let Out (r, s) = g y
   in Out (r, mappend (mappend w  w') s)
 
 

 
 
 
 
 
 
 
 
 
 
 
 
	
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
