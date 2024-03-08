module Assembler (Instruction (..), assemble) where

import AST
import Data.List.NonEmpty (NonEmpty (..), append, init)
import Prelude hiding (init)

data Instruction where
  IM :: Int -> Instruction -- load the constant value n into R0
  AR :: Int -> Instruction -- load the n-th input argument into R0
  SW :: Instruction -- swap R0 and R1
  PU :: Instruction -- push R0 onto the stack
  PO :: Instruction -- pop the top value off of the stack into R0
  AD :: Instruction -- add R1 to R0 and put the result into R0
  SU :: Instruction -- subtract R1 from R0 and put the result in R0
  MU :: Instruction -- multiply R0 by R1 and put the result in R0
  DI :: Instruction -- divide R0 by R1 and put the result in R0
  deriving (Show, Eq)

assemble :: AST -> [Instruction]
assemble ast = optimize $ init $ assemble' ast

optimize :: [Instruction] -> [Instruction]
optimize [] = []
optimize [x] = [x]
optimize (PU : PO : ys) = optimize ys
optimize (x : ys) = x : optimize ys

assemble' :: AST -> NonEmpty Instruction
assemble' ast = case ast of
  Imm num -> IM num :| [PU]
  Arg num -> AR num :| [PU]
  Add x y -> append (assemble'' x y) (operate AD)
  Sub x y -> append (assemble'' x y) (operate SU)
  Mul x y -> append (assemble'' x y) (operate MU)
  Div x y -> append (assemble'' x y) (operate DI)
  where
    assemble'' x y = append (assemble' x) (assemble' y)
    operate ins = PO :| [SW, PO, ins, PU]
