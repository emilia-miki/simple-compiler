module Parser (parse) where

import AST
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Token

type Arguments = M.Map String Int

parseArgs :: [Token] -> (Arguments, [Token])
parseArgs tokens = (M.fromList $ zip argNames [0 ..], rest)
  where
    argNames = map liftArg $ takeWhile (/= TChar ']') $ tail tokens
    rest = tail $ dropWhile (/= TChar ']') tokens
    liftArg token = case token of
      TStr arg -> arg
      _other -> undefined

parseFactor :: Arguments -> [Token] -> (AST, [Token])
parseFactor _ [] = undefined
parseFactor args (t : ts) = case t of
  TInt num -> (Imm num, ts)
  TStr arg -> (Arg $ fromJust $ M.lookup arg args, ts)
  TChar '(' -> (fst $ parseExpression args (takeParens (t : ts)), skipParens (t : ts))
  _other -> undefined

skipParens :: [Token] -> [Token]
skipParens tokens = go tokens 0
  where
    go :: [Token] -> Int -> [Token]
    go (t : ts) 0 = case t of
      TChar '(' -> go ts 1
      _other -> t : ts
    go (t : ts) n = case t of
      TChar '(' -> go ts (n + 1)
      TChar ')' -> go ts (n - 1)
      _other -> go ts n
    go [] 0 = []
    go [] _ = undefined

takeParens :: [Token] -> [Token]
takeParens tokens = reverse $ go tokens 0 []
  where
    go :: [Token] -> Int -> [Token] -> [Token]
    go (t : ts) 0 [] = case t of
      TChar '(' -> go ts 1 []
      _other -> undefined
    go (t : ts) 1 res = case t of
      TChar '(' -> go ts 2 (TChar '(' : res)
      TChar ')' -> res
      other -> go ts 1 (other : res)
    go (t : ts) n res = case t of
      TChar '(' -> go ts (n + 1) (TChar '(' : res)
      TChar ')' -> go ts (n - 1) (TChar ')' : res)
      other -> go ts n (other : res)
    go [] n res = undefined

parseTerm :: Arguments -> [Token] -> (AST, [Token])
parseTerm args tokens =
  let (f1, tokens1) = parseFactor args tokens
   in go f1 tokens1
  where
    go acc (op : tokens2)
      | op == TChar '*' || op == TChar '/' =
          let (f2, tokens3) = parseFactor args tokens2
           in go (astOp op acc f2) tokens3
    go acc rest = (acc, rest)
    astOp (TChar '*') = Mul
    astOp (TChar '/') = Div
    astOp _ = undefined

parseExpression :: Arguments -> [Token] -> (AST, [Token])
parseExpression args tokens =
  let (f1, tokens1) = parseTerm args tokens
   in go f1 tokens1
  where
    go acc (op : tokens2)
      | op == TChar '+' || op == TChar '-' =
          let (f2, tokens3) = parseTerm args tokens2
           in go (astOp op acc f2) tokens3
    go acc rest = (acc, rest)
    astOp (TChar '+') = Add
    astOp (TChar '-') = Sub
    astOp _ = undefined

parse :: [Token] -> AST
parse tokens = fst $ parseExpression args tokens1
  where
    args' = parseArgs tokens
    args = fst args'
    tokens1 = snd args'
