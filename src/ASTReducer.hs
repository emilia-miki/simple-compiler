module ASTReducer (astReduce) where

import AST

astReduce :: AST -> AST
astReduce ast = case ast of
  ASTUndefined -> ASTUndefined
  Imm x -> Imm x
  Arg x -> Arg x
  Add (Imm x) (Imm y) -> Imm (x + y)
  Sub (Imm x) (Imm y) -> Imm (x - y)
  Mul (Imm x) (Imm y) -> Imm (x * y)
  Div (Imm x) (Imm y) -> Imm (x `div` y)
  Add x y -> astReduce' $ Add (astReduce x) (astReduce y)
  Sub x y -> astReduce' $ Sub (astReduce x) (astReduce y)
  Mul x y -> astReduce' $ Mul (astReduce x) (astReduce y)
  Div x y -> astReduce' $ Div (astReduce x) (astReduce y)
  where
    astReduce' ast' = case ast' of
      Add (Imm x) (Imm y) -> Imm (x + y)
      Sub (Imm x) (Imm y) -> Imm (x - y)
      Mul (Imm x) (Imm y) -> Imm (x * y)
      Div (Imm x) (Imm y) -> Imm (x `div` y)
      other -> other
