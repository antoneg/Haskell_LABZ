
module Expr where

import Test.QuickCheck
import Data.Char
import System.Random
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Data.String
import Parsing
import Data.Tuple


--A

data Expr = Var | Num Double | Op Opperand Expr Expr | Func MathFunc Expr
  deriving (Eq, Show)

data MathFunc = Cos | Sin 
  deriving (Eq, Show)

data Opperand = Mul | Add 
   deriving (Eq, Show)
    

e1 = Op Add (Num 5) (Op Mul (Op Add (Num 3) (Num 2)) Var)
ez = Op Add (Func Sin (Op Add (Op Mul (Num 2) (Var)) (Num 3) ) ) (Op Add (Op Mul (Num 3) (Var)) (Num 5))
em = Op Mul (Op Mul (Num 1) (Num 2)) (Op Mul (Num 3) (Num 4)) 
ecis = Func Sin (Func Cos (Op Add (Num 3) Var) )
ecis2 = Func Sin (Func Cos (Op Mul (Num 3) Var) )
ecis3 = Func Sin (Func Cos (Num 4) )

opString :: Opperand -> String
opString Mul = "*"
opString Add = "+"

funcString :: MathFunc -> String
funcString Sin = "sin"
funcString Cos = "cos"

--B

showExpr :: Expr -> String
showExpr (Num e)  = show e
showExpr Var = "x"
showExpr (Op Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Op op e1 e2) = showExpr e1 ++ opString op ++ showExpr e2
showExpr (Func f e) = funcString f ++ showFunc e

showFactor :: Expr -> String
showFactor (Op Add e1 e2) ="("++ showExpr (Op Add e1 e2) ++ ")"
showFactor Var = showExpr Var
showFactor e = showExpr e

showFunc :: Expr -> String
showFunc (Op op e1 e2) ="("++ showExpr (Op op e1 e2) ++ ")"
showFunc (Func f e) = "(" ++ showExpr (Func f e) ++ ")"
showFunc e = showExpr e

eval :: Expr -> Double -> Double
eval Var d = d
eval (Num n) d = n
eval (Op op e1 e2) d = evalOp op (eval e1 d) (eval e2 d)
eval (Func func e2) d = evalFunc func (eval e2 d)

evalOp :: Opperand -> Double -> Double -> Double
evalOp Add = (+)
evalOp Mul = (*)

evalFunc :: MathFunc -> Double -> Double
evalFunc Sin = sin
evalFunc Cos = cos

--D 

readExpr :: String -> Maybe Expr
readExpr s =
  case parse expr (filter (not.isSpace) s) of
    Just (strExrp,"") -> Just strExrp
    _                 -> Nothing

expr, term, factor :: Parser Expr
expr = do
  t <- term
  ts <- zeroOrMore (do char '+'; term)
  return $ foldl (Op Add) t ts

term  = do
  t <- factor
  ts <- zeroOrMore (do char '*'; factor)
  return $ foldl (Op Mul) t ts

factor = number <|> parenthesis <|> variable <|> sinus <|> cosinus

number = Num <$> readsP

parenthesis = do char '('
                 e <- expr
                 char ')'
                 return e

variable :: Parser Expr
variable = char 'x' *> return Var

cosinus = Func Cos <$> ((c1 <|> c2) *> (o1 <|>  o2) *> (s1 <|> s2) *> factor)
sinus = Func Sin <$> ((s1 <|> s2) *> (i1 <|>  i2) *> (n1 <|> n2) *> factor)
c1 = do char 'C'
c2 = do char 'c'
o1 = do char 'O'
o2 = do char 'o'
s1 = do char 'S'
s2 = do char 's'
i1 = do char 'I'
i2 = do char 'i'
n1 = do char 'N'
n2 = do char 'n'

-- E 

prop_ShowReadExpr' :: Expr -> Bool
prop_ShowReadExpr' e = showExpr e == showExpr (fromJust (readExpr (showExpr e)))

instance Arbitrary Expr where arbitrary = do n <- choose (1,5); genExpr n

genExpr :: Int -> Gen (Expr)
genExpr 0 = oneof [do n <- choose(1,10); return (Num n), do return Var]
genExpr n = oneof[ do op <- elements[Op Add, Op Mul]
                      l <- choose(0,n-1)
                      let r = n-1-l
                      e1 <- genExpr l
                      e2 <- genExpr r
                      return(op e1 e2),
                   do op <- elements[Func Sin, Func Cos]
                      l <- choose(0,n-1)
                      e1 <- genExpr l
                      return(op e1)]

-- F

simplify :: Expr -> Expr

simplify (Op Add (Num n1) (Num n2)) = Num (n1+n2)
simplify (Op Add (Num 0) e) = simplify e
simplify (Op Add e (Num 0)) = simplify e
simplify (Op Add (Op Add e1 e2) e3) = simplify (Op Add e1 (Op Add e2 e3))
simplify (Op Add e1 e2) = (Op Add (simplify e1) (simplify e2))

simplify (Op Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify (Op Mul (Num 0) e) = Num 0
simplify (Op Mul e (Num 0)) = Num 0
simplify (Op Mul (Num 1) e) = simplify e
simplify (Op Mul e (Num 1)) = simplify e
simplify (Op Mul (Op Mul e1 e2) e3) = simplify (Op Mul e1 (Op Mul e2 e3))
simplify (Op Mul e1 e2) = (Op Mul (simplify e1) (simplify e2))

simplify (Func f e) = (Func f (simplify e))

simplify (Num n) = Num n
simplify Var = Var

-- G

differentiate :: Expr -> Expr
differentiate (Var) = (Num 1)
differentiate (Num n) = (Num 0)
differentiate (Func Sin e) = simplify(Op Mul (Func Cos e) (differentiate e))
differentiate (Func Cos e) = simplify(Op Mul (Op Mul (Func Sin e) (differentiate e)) (Num (-1)))
differentiate (Op Add e1 e2) = simplify(Op Add (differentiate e1) (differentiate e2))
differentiate (Op Mul e1 e2) = simplify(Op Add (Op Mul (differentiate e1) e2) (Op Mul e1 (differentiate e2)))