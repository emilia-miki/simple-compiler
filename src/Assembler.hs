module Assembler (Instruction (..), assemble) where

import AST

-- "IM n"     // load the constant value n into R0
-- "AR n"     // load the n-th input argument into R0
-- "SW"       // swap R0 and R1
-- "PU"       // push R0 onto the stack
-- "PO"       // pop the top value off of the stack into R0
-- "AD"       // add R1 to R0 and put the result in R0
-- "SU"       // subtract R1 from R0 and put the result in R0
-- "MU"       // multiply R0 by R1 and put the result in R0
-- "DI"       // divide R0 by R1 and put the result in R0

assemble :: AST -> [Instruction]
assemble ast = optimize $ init $ assemble' ast

optimize :: [Instruction] -> [Instruction]
optimize [] = []
optimize [x] = [x]
optimize (PU : PO : ys) = optimize ys
optimize (x : ys) = x : optimize ys

assemble' :: AST -> [Instruction]
assemble' ast = case ast of
  ASTUndefined -> undefined
  Imm num -> [IM num, PU]
  Arg num -> [AR num, PU]
  Add x y -> assemble' x ++ assemble' y ++ [PO, SW, PO, AD, PU]
  Sub x y -> assemble' x ++ assemble' y ++ [PO, SW, PO, SU, PU]
  Mul x y -> assemble' x ++ assemble' y ++ [PO, SW, PO, MU, PU]
  Div x y -> assemble' x ++ assemble' y ++ [PO, SW, PO, DI, PU]

data Instruction where
  IM :: Int -> Instruction
  AR :: Int -> Instruction
  SW :: Instruction
  PU :: Instruction
  PO :: Instruction
  AD :: Instruction
  SU :: Instruction
  MU :: Instruction
  DI :: Instruction
  deriving (Show, Eq)
