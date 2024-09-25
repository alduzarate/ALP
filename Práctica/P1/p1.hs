import Parsing
import Data.Char (digitToInt)
import Control.Monad
import Control.Applicative hiding (many)
import Foreign.C (CFloat)
import Language.Haskell.TH (Con)

-- 1)
-- La primitiva sepBy lo que hace es tomar 2 parsers y los va aplicando alternadamente, comenzando por el primero pasado
-- como parámetro. Algunos ejemplos:

-- ghci> parse (sepBy digit space) "0 1 2 3 4"
-- [("01234","")]
-- ghci> parse (sepBy digit space) "0     1 2 3 4"
-- [("01234","")]
-- ghci> parse (sepBy space digit) "0 1 2 3 4"
-- [([(),(),(),(),(),()],"")]
-- ghci> parse (sepBy digit space) "0 1 a 3 4"
-- [("01"," a 3 4")]
-- ghci> parse (sepBy alphanum space) "0 1 a 3 4"
-- [("01a34","")]

-- La primitiva symbol es una extensión del parser string, ya que parsea una palabra específica ignorando todos
-- los espacios que pueda haber hasta ella. Algunos ejemplos:
-- ghci> parse (symbol "hola") "hola"
-- [("hola","")]
-- ghci> parse (symbol "hola") "      hola"
-- [("hola","")]
-- ghci> parse (symbol "hola") "      hol a"
-- []
-- ghci> parse (symbol "hola") "      hol a   hola"
-- []

-- 2)

expr :: Parser Int 
expr = do t <- term
          (do char '+'
              e <- expr
              return (t + e)
            <|> do  char '-'
                    e <- expr
                    return (t + e))
              <|> return t

term :: Parser Int
term = do f <- factor 
          (do char '*'
              t <- term
              return (f * t)
            <|> do  char '/'
                    t <- term
                    return (f `div` t))
              <|> return f

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
         <|> do char '('
                e <- expr
                char ')'
                return e


eval :: String -> Int
eval xs = fst (head (parse expr xs))

-- 3)
parenthesisString :: Parser a -> Parser a
parenthesisString p = do  char '('
                          r <- p
                          char ')'
                          return r
                      <|> do  r <- p
                              return r

-- 4)

data Expr = Num Int | BinOp Op Expr Expr deriving (Show)
data Op = Add | Mul | Min | Div deriving (Show)

expr' :: Parser Expr
expr' = do  t <- term'
            (do char '+'
                e <- expr'
                return (BinOp Add t e)
              <|> do  char '-'
                      e <- expr'
                      return (BinOp Min t e))
              <|> return t

term' :: Parser Expr
term' = do  f <- factor' 
            (do char '*'
                t <- term'
                return (BinOp Mul f t)
              <|> do  char '/'
                      t <- term'
                      return (BinOp Div f t))
              <|> return f

factor' :: Parser Expr
factor' = do  d <- digit
              return (Num (digitToInt d))
          <|> do  char '('
                  e <- expr'
                  char ')'
                  return e

astExpr :: String -> Expr
astExpr xs = fst (head (parse expr' xs))

-- 5)

data Basetype = DInt | DChar | DFloat deriving (Show)
type Hasktype = [Basetype]

haskellTypes :: Parser Hasktype
haskellTypes = sepBy htype arrow
  where
    htype = do  string "Int"
                return DInt
              <|> do  string "Char"
                      return DChar
              <|> do  string "Float"
                      return DFloat
    arrow = token (string "->")

-- 6)
data HElem = HInt Int | HChar Char deriving (Show)
type HLists = [HElem]

hlists :: Parser HLists
hlists = do char '['
            l <- sepBy helem comma
            char ']'
            return l
  where
    helem = do  d <- digit
                return (HInt (digitToInt d))
              <|> do  char '\''
                      c <- letter
                      char '\''
                      return (HChar c)
    comma = token (char ',')

hlistparse :: String -> HLists
hlistparse xs = fst (head (parse hlists xs))

-- 7)

{-
La gramatica está dada por:
Htype -> Base | Fun
Fun -> Base '->' Base | Base '->' Fun
Base -> DInt | DChar | DFloat

-}
data HasktypeRS = DIntRS | DCharRS | DFloatRS | Fun HasktypeRS HasktypeRS deriving (Show)

hasktypeRSparser :: Parser HasktypeRS
hasktypeRSparser = do b <- base
                      (do token (string "->")
                          r <- hasktypeRSparser
                          return (Fun b (r))
                        <|> return b)

base :: Parser HasktypeRS
base = do string "Int"
          return DIntRS
        <|> (do string "Char"
                return DCharRS
              <|> do  string "Float"
                      return DFloatRS)

haskellRSparse :: String -> HasktypeRS
haskellRSparse xs = fst (head (parse hasktypeRSparser xs))

-- 8) REPASAR
{-
expr quedaría
expr -> term expr'
expr' -> \eps | (’+’ term | ’-’ term) expr'
expr' -> \eps | '+' term expr' | '-' term expr'

term quedaría
term -> factor term'
term' -> \eps | (’*’ factor | ’/’ factor ) term'
term' -> \eps | '*' factor term' | '/' factor term'

factor -> digit | '(' expr ')'
digit -> '0' | '1' | '2' | '3' | '4' | '5' | ... | '9'
-}

expri :: Parser Int
expri = do  t <- termi
            e <- expri'
            return (e t)

expri' :: Parser (Int -> Int)
expri' = do char '+'
            t <- term
            e <- expri'
            return (e . (+t))
          <|> do  char '-'
                  t <- term
                  e <- expri'
                  return (e . (\x -> x - t))
          <|> return id

termi :: Parser Int
termi = do  f <- factor
            t <- termi'
            return (t f)

termi' :: Parser (Int -> Int)
termi' = do char '*'
            f <- factor
            t <- termi'
            return (t . (*f))
          <|> do  char '/'
                  f <- factor
                  t <- termi'
                  return (t . (\x -> x `div` f))
          <|> return id

evali :: String -> Int
evali xs = fst (head (parse expri xs))

-- 9)

{-
La regla direct_declarator tiene recursión a izquierda, con lo cual tendremos que transformarla para quitársela:

direct_declarator -> '(' direct_declarator ')' direct_declarator'| identifier direct_declarator'
direct_declarator' -> \eps | '[' constant expression ']' direct_declarator'

Ejemplo de algo que tendría que poder parsear esto: int *a[10];
-}
type ConstantExpression = Int

data CTypeSpec = CInt | CChar | CFloat 
  deriving (Show)

data CDirectDeclarator = CArray CDirectDeclarator ConstantExpression | CParen CDirectDeclarator | CVar String
  deriving (Show)

data CDeclarator = CPointer CDeclarator | CDeclarator CDirectDeclarator
  deriving (Show)

data CDeclaration = CDecl CTypeSpec CDeclarator
  deriving (Show)

cdeclaration :: Parser CDeclaration
cdeclaration = do t <- ctypespec
                  cd <- cdeclarator
                  symbol ";"
                  return (CDecl t cd)

cdeclarator :: Parser CDeclarator
cdeclarator = do  symbol "*"
                  d <- cdeclarator
                  return (CPointer d)
                <|> do  dd <- cdirectdeclarator
                        return (CDeclarator dd)

{-
direct_declarator -> '(' direct_declarator ')' direct_declarator'| identifier direct_declarator'
direct_declarator' -> \eps | '[' constant expression ']' direct_declarator'
-}
cdirectdeclarator :: Parser CDirectDeclarator
cdirectdeclarator = do  symbol "("
                        dd <- cdirectdeclarator
                        symbol ")"
                        dd' <- cdirectdeclarator'
                        return (CParen (dd' dd))
                      <|> do  v <- identifier
                              dd' <- cdirectdeclarator'
                              return (dd' (CVar v))

cdirectdeclarator' :: Parser (CDirectDeclarator -> CDirectDeclarator)
cdirectdeclarator' = do symbol "["
                        ce <- constexpression
                        symbol "]"
                        dd <- cdirectdeclarator'
                        return (\idd-> (CArray (dd idd) ce))
                      <|> return id

ctypespec :: Parser CTypeSpec
ctypespec = do  symbol "int"
                return CInt
            <|> do  symbol "char"
                    return CChar
                <|> do  symbol "float"
                        return CFloat

constexpression :: Parser ConstantExpression
constexpression = do  d <- digit
                      return (digitToInt d)

