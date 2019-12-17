
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



--num :: Double -> Expr
--add,mul :: Expr -> Expr -> Expr
--sin,cos :: Expr -> Expr

--A
data Expr = Var | Num Double | Add Expr Expr | Mul Expr Expr | Sin Expr | Cos Expr
  deriving (Eq, Show)

e1 = Add (Num 5) (Mul (Add (Num 3) (Num 2)) Var)
ez = Add (Sin (Add (Mul (Num 2) (Var)) (Num 3) ) ) (Add (Mul (Num 3) (Var)) (Num 5))

-- B
showExpr :: Expr -> String
showExpr (Num e)  = show e
showExpr Var = "x"
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Cos e) = "cos" ++ showTri e
showExpr (Sin e) = "sin" ++ showTri e

showFactor :: Expr -> String
showFactor (Add e1 e2) ="("++ showExpr (Add e1 e2) ++ ")"
showFactor Var = showExpr Var
showFactor e = showExpr e

showTri :: Expr -> String
showTri (Add e1 e2) ="("++ showExpr (Add e1 e2) ++ ")"
showTri (Mul e1 e2) ="("++ showExpr (Mul e1 e2) ++ ")"
showTri e = showExpr e

-- C
eval :: Expr -> Double -> Double
eval Var d = d
eval (Num n) d = n
eval (Add e1 e2) d = (eval e1 d) + (eval e2 d)
eval (Mul e1 e2) d = eval e1 d * eval e2 d
eval (Sin e2) d = sin (eval e2 d)
eval (Cos e2) d = cos (eval e2 d)


-- D
readExpr :: String -> Maybe Expr
readExpr s =
  case parse expr (filter (not.isSpace) s) of
    Just (strExrp,"") -> Just strExrp
    _                 -> Nothing

expr, term, factor :: Parser Expr
expr = do
  t <- term
  ts <- zeroOrMore (do char '+'; term)
  return $ foldl Add t ts

term  = do
  t <- factor
  ts <- zeroOrMore (do char '*'; factor)
  return $ foldl Mul t ts

factor = number <|> parenthesis <|> variable <|> sinus <|> cosinus

number = Num <$> readsP

parenthesis = do char '('
                 e <- expr
                 char ')'
                 return e

variable :: Parser Expr
variable = char 'x' *> return Var

cosinus = Cos <$> ((c1 <|> c2) *> (o1 <|>  o2) *> (s1 <|> s2) *> factor)
sinus = Sin <$> ((s1 <|> s2) *> (i1 <|>  i2) *> (n1 <|> n2) *> factor)
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
genExpr n = oneof[ do op <- elements[Add,Mul]
                      l <- choose(0,n-1)
                      let r = n-1-l
                      e1 <- genExpr l
                      e2 <- genExpr r
                      return(op e1 e2),
                   do op <- elements[Sin,Cos]
                      l <- choose(0,n-1)
                      e1 <- genExpr l
                      return(op e1)]

-- F
simplify :: Expr -> Expr

simplify (Add (Num n1) (Num n2)) = Num (n1+n2)
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Add (Add e1 e2) e3) = simplify (Add e1 (Add e2 e3))
simplify (Add e1 e2) = (Add (simplify e1) (simplify e2))

simplify (Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify (Mul (Num 0) e) = Num 0
simplify (Mul e (Num 0)) = Num 0
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Mul (Mul e1 e2) e3) = simplify (Mul e1 (Mul e2 e3))
simplify (Mul e1 e2) = (Mul (simplify e1) (simplify e2))

simplify (Sin e) = (Sin (simplify e))
simplify (Cos e) = (Cos (simplify e))

simplify (Num n) = Num n
simplify Var = Var

-- G

differentiate :: Expr -> Expr
differentiate (Var) = (Num 1)
differentiate (Num n) = (Num 0)
differentiate (Sin e) = simplify(Mul (Cos e) (differentiate e))
differentiate (Cos e) = simplify(Mul (Mul (Sin e) (differentiate e)) (Num (-1)))
differentiate (Add e1 e2) = simplify(Add (differentiate e1) (differentiate e2))
differentiate (Mul e1 e2) = simplify(Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2)))

