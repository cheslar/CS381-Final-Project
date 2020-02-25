module StackyStack where

import Prelude hiding (Num)

-- | Abstract syntax
type Prog = [Cmd]
data Expr = Num Int
          | Stack
          | Cat Prog
          | Add
          | Mul
          | Equ
          | Lt
          | Swap
          | Dup
          | Over
          | Drop
          | Shift
          | Extend
          deriving(Eq, Show)

data Cmd = Push Expr
         | While Prog
         | IfElse Prog Prog
         deriving(Eq, Show)

-- | Semantic domain
data StackItem = Primitive Int
               | Object Stack
               deriving(Eq, Show)
type Stack = [StackItem]
type Domain = Stack -> Maybe Stack

expr :: Expr -> Domain
expr (Num x) s = Just (Primitive x : s)
expr (Stack) s = Just (Object [] : s)
expr (Cat p) ((Object x):s) = case prog p x of
                                Just x' -> Just (Object x' : s)
                                _ -> Nothing
expr (Add) (Primitive x : Primitive y : s) = Just (Primitive (x + y) : s)
expr (Add) _ = Nothing
expr (Mul) (Primitive x : Primitive y : s) = Just (Primitive (x * y) : s)
expr (Mul) _ = Nothing
expr (Equ) (Primitive x : Primitive y : s) = Just (Primitive (if x == y then 1 else 0) : s)
expr (Equ) _ = Nothing
expr (Lt) (Primitive x : Primitive y : s) = Just (Primitive (if x < y then 1 else 0) : s)
expr (Lt) _ = Nothing
expr (Swap) (x:y:s) = Just (y:x:s)
expr (Dup) (x:s) = Just (x:x:s)
expr (Over) (x:y:s) = Just (y:x:y:s)
expr (Drop) (x:s) = Just s
expr (Shift) (x:s) = Just (s ++ [x])
expr (Extend) ((Object x):s) = Just (x ++ s)
expr (Extend) _ = Nothing




cmd :: Cmd -> Domain
cmd (Push e) s = expr e s
cmd (While _) ((Primitive 0):s) = Just (Primitive 0 : s)
cmd (While p) ((Primitive x):s) = case prog p ((Primitive x):s) of
                                    Just s' -> cmd (While p) s'
                                    _ -> Nothing
cmd (IfElse l r) ((Primitive 0):s) = prog r ((Primitive 0):s)
cmd (IfElse l r) ((Primitive x):s) = prog l ((Primitive x):s)



prog :: Prog -> Domain
prog [] s = Just s
prog (c:p) s = case cmd c s of
                 Just s' -> prog p s'
                 _ -> Nothing


run :: Prog -> Maybe Stack
run p = prog p []


-- | Syntactic sugar
true :: Expr
true = Num 1

false :: Expr
false = Num 0

exec :: Prog -> Cmd
exec p = IfElse p p

neg :: Cmd
neg = exec [Push (Num (-1)), Push Mul]

and :: Cmd
and = exec [Push true, Push Equ, Push Equ]

or :: Cmd
or = exec [Push true, Push Equ, IfElse [Push true] [Push Equ]]

makeStack :: 
