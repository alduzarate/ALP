import Control.Monad
import Control.Applicative (Applicative(..))

newtype Output w a = Out (a,w)


runOut :: Monoid w => (Output w a) -> (a,w)
runOut (Out (a,s)) = (a,s)

instance Monoid w => Monad (Output w) where
 return x = Out (x,mempty)
 m >>= f  = Out (let (x,w)  = runOut m
                     (y,w') = runOut (f x)
                 in (y, mappend w w'))
                 

instance Monoid w => Functor (Output w) where
    fmap = liftM
 
instance Monoid w => Applicative (Output w) where
    pure   = return
    (<*>)  = ap 

write :: Monoid w => w -> Output w ()
write w = Out ((),w)


data Exp = Const Int | Plus Exp Exp | Div Exp Exp

evalM1 :: Exp -> Output String Int
evalM1 (Const n)  = do write "El termino (Const "
                       write (show n)
                       write ") tiene valor "
                       write (show n)
                       return n
evalM1 (Plus t u) = do m <- evalM1 t
                       n <- evalM1 u 
                       return(m+n)
evalM1(Div  t u)  = do m <- evalM1 t 
                       n <- evalM1 u
                       write "El termino (Div (Const "
                       write (show m)
                       write ") (Const "
                       write (show n)
                       write ")) tiene valor "
                       write (show (m `div` n))
                       return(m `div` n)
                       
                       

-- data T = Con Int | Div T T

-- newtype M s e a = M {runM :: s -> Error e (a,s)}



-- ~ eval    :: T -> M Int String Int
-- ~ eval (Con n)     = return n
-- ~ eval (Div t1 t2) = do v1 <- eval t1
                      -- ~ v2 <- eval t2
                      -- ~ if (v2 == 0) <- eval t2
