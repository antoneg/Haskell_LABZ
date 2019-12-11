module Expr where

import Test.QuickCheck
import Data.Char
import System.Random
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Data.String


--x :: Expr
--x = 'x'
--num :: Double -> Expr
--add,mul :: Expr -> Expr -> Expr
--sin,cos :: Expr -> Expr 

data Expr = Var String | Num Integer | Add Expr Expr | Mul Expr Expr | Sin Expr | Cos Expr
  deriving (Eq, Show)

e1 = Add (Num 5) (Num 3)
ez = Add (Sin (Add (Mul (Num 2) (Var "x")) (Num 3) ) ) (Add (Mul (Num 3) (Var "x")) (Num 5)) 


showExpr :: Expr -> String
showExpr (Num e)  = show e
showExpr (Var x) = "x"
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2 
showExpr (Cos e) = "cos" ++ showExpr e

showFactor :: Expr -> String 
showFactor (Add e1 e2) ="("++ showExpr (Add e1 e2) ++ ")"
showFactor e = showExpr e
