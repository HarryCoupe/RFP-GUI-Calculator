{- Used from Compilers course COMP3012-}

module Parser where

import Control.Applicative
import Data.Char

--RFP Code

--Function to reverse the string
revString :: Parser ()
revString = P (\rev -> [((),reverse rev)])

--Parses the string for the last number and checks if it is a negative or not
lastNum :: Parser Double
lastNum = do revString 
             b <- some (digit <|> char '.')
             let d = read(reverse b) :: Double
             (do char '-'
                 (do sat' opper
                     revString
                     return (negate d) 
                  <|>
                  do checkEmpty
                     revString
                     return (negate d)
                  <|>
                  do revString
                     return d)
              <|>
              do revString
                 return d)

--Used in sat' to check for a symbol but not take it out of the string
checkSym :: Parser Char
checkSym = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,x:xs)])

--Used to check if after the string is empty when checking negatives
checkEmpty :: Parser ()
checkEmpty = P (\inp -> case inp of
                     []     -> [((),"")]
                     (x:xs) -> [])

--Used to check a symbol without removing it from the string like sat
sat' :: (Char -> Bool) -> Parser Char
sat' = satisfy checkSym

--Used to look for any other opperation after a '-' when checking for negatives
opper :: Char -> Bool
opper '+' = True 
opper '-' = True
opper '*' = True
opper '/' = True 
opper _   = False

--COMP3012 - Compilers Code

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data AST = LitInteger Double
         | BinOp BinOperator AST AST
         | UnOp  UnOperator AST
         | Conditional AST AST AST
  deriving (Eq,Show)

-- Parse the top string: error if parsers fail or input not consumed

expParse :: String -> AST
expParse src = case parse expr src of
  [(t,"")] -> t
  _ -> error "Parsing error"



expr :: Parser AST
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)

bexp :: Parser AST
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser AST
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser AST
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser AST
aexp = aexp' id

aexp' :: (AST -> AST) -> Parser AST
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser AST
mexp = mexp' id

mexp' :: (AST -> AST) -> Parser AST
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser AST
aterm = (natural >>= return . LitInteger)
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr

--FunParser
-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = P (\src -> [ (g x, src1) | (x,src1) <- parse pa src ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\src -> [(x,src)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P (\src -> [ (f x,src2) | (f,src1) <- parse pf src,
                                        (x,src2) <- parse pa src1 ] )

instance Monad Parser where
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = P (\src -> [r | (x,src1) <- parse pa src,
                               r <- parse (fpb x) src1 ] )

--Making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\rsc -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\src -> case parse p1 src of
                    [] -> parse p2 src
                    rs -> rs)

-- Chosing among many alternatives
choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

{-
Parallel parsing: getting the results of both parsers
  for ambiguous grammars
Use with caution: it can cause inefficiency
-}

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> (parse p1 inp) ++ (parse p2 inp))

-- Derived primitives

-- verify that the parsed object satisfy a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item


 
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: (Num int, Read int) => Parser int
nat = do xs <- some (digit <|> char '.')
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

-- Example: parsing a list of integers
nats :: Parser [Integer]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: Double -> Double -> Double
intAND = boolInt .< (&&) <. intBool

intOR :: Double -> Double -> Double
intOR = boolInt .< (||) <. intBool

intNOT :: Double -> Double
intNOT = boolInt . not . intBool

-- Correspondence between Booleans and integers
boolInt :: Bool -> Double
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: Double -> Bool
intBool x = x/=0

-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)

--ExpCompiler
-- Directly evaluate the input expression

eval :: String -> String
eval s = show (evaluate (expParse s))
-- Evaluator for AST

evaluate :: AST -> Double
evaluate (LitInteger x)   = x
evaluate (BinOp op t1 t2) = binOpEv op (evaluate t1) (evaluate t2)
evaluate (UnOp op t)      = unOpEv op (evaluate t)
evaluate (Conditional b t1 t2) = if (evaluate b) /= 0
                                 then (evaluate t1)
                                 else (evaluate t2)

binOpEv :: BinOperator -> Double -> Double -> Double
binOpEv Addition       = (+)
binOpEv Subtraction    = (-)
binOpEv Multiplication = (*)
binOpEv Division       = (/)
binOpEv Conjunction    = intAND
binOpEv Disjunction    = intOR
binOpEv LssOp          = relInt (<)
binOpEv LeqOp          = relInt (<=)
binOpEv GtrOp          = relInt (>)
binOpEv GeqOp          = relInt (>=)
binOpEv EqOp           = relInt (==)
binOpEv NeqOp          = relInt (/=)

-- Boolean relation that returns an integer
relInt :: (Double -> Double -> Bool) -> Double -> Double -> Double
relInt rel = boolInt .< rel

unOpEv :: UnOperator -> Double -> Double
unOpEv Negation = negate
unOpEv NegBool  = intNOT

