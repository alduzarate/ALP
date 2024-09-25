import Control.Monad
import Control.Applicative (Applicative(..))
import System.IO
import Data.Char

printState :: [Int] -> IO ()    
printState []     = putStr "\n"
printState (x:xs) = do putStrLn (putStars x)
                       printState xs

putStars :: Int -> String
putStars 0 = ""
putStars n = "* " ++ putStars (n-1)

takeStars :: Int -> Int -> [Int] -> [Int]
takeStars pos cant xs = let x = xs!!pos
                        in if (cant >= x) then take pos xs ++ [0] ++ drop (pos+1) xs
                                          else take pos xs ++ [(x-cant)] ++ drop (pos+1) xs

nim :: [Int] -> IO ()
nim xs = do putStrLn "Estado actual de nim"
            printState xs
            putStrLn "Escriba posicion a tomar"
            pos <- getLine 
            putStrLn "Escriba cantidad a tomar"
            cant <- getLine
            let ys = (takeStars ((read pos)-1) (read cant) xs) in 
              if (sum ys == 0) then putStrLn "Se termino" else nim ys  


filer :: String -> IO ()
filer s = do st <-readFile s
             writeFile "Suspenso.txt" (map toUpper st)
