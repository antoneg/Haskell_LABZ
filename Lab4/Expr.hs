module Expr where

import Test.QuickCheck
import Data.Char
import System.Random
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Data.String
import Parsing


x :: Expr
x = Var "x"
--num :: Double -> Expr
--add,mul :: Expr -> Expr -> Expr
--sin,cos :: Expr -> Expr 

--A
data Expr = Var String | Num Double | Add Expr Expr | Mul Expr Expr | Sin Expr | Cos Expr
  deriving (Eq, Show)

e1 = Add (Num 5) (Num 3)
ez = Add (Sin (Add (Mul (Num 2) (Var "x")) (Num 3) ) ) (Add (Mul (Num 3) (Var "x")) (Num 5)) 

 -- B
showExpr :: Expr -> String
showExpr (Num e)  = show e
showExpr (Var x) = "x"
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2 
showExpr (Cos e) = "cos" ++ showTri e
showExpr (Sin e) = "Sin" ++ showTri e

showFactor :: Expr -> String 
showFactor (Add e1 e2) ="("++ showExpr (Add e1 e2) ++ ")"
showFactor e = showExpr e

showTri :: Expr -> String 
showTri (Add e1 e2) ="("++ showExpr (Add e1 e2) ++ ")"
showTri (Mul e1 e2) ="("++ showExpr (Mul e1 e2) ++ ")"
showTri e = showExpr e

-- C 
eval :: Expr -> Double -> Double
eval (Var v) d = d 
eval (Num n) d = n
eval (Add e1 e2) d = (eval e1 d) + (eval e2 d)
eval (Mul e1 e2) d = eval e1 d * eval e2 d 
eval (Sin e2) d = sin (eval e2 d)
eval (Cos e2) d = cos (eval e2 d)

--readExpr :: String -> Maybe Expr
