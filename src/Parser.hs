module Parser (parse) where

import AST
import Data.List.NonEmpty (NonEmpty (..), dropWhile, nonEmpty, tail)
import Data.Map qualified as M
import Token

type Arguments = M.Map String Int

parseArgs :: NonEmpty Token -> (Arguments, [Token])
parseArgs tokens = (M.fromList $ zip argNames [0 ..], rest)
  where
    argNames = map liftArg $ takeWhile (/= TChar ']') $ Data.List.NonEmpty.tail tokens
    rest = case Data.List.NonEmpty.dropWhile (/= TChar ']') tokens of
      [] -> error "expected ], but got end of input"
      (TChar ']' : ts) -> ts
      _impossible -> error "this is impossible"
    liftArg token = case token of
      TStr arg -> arg
      other -> error ("invalid token in arguments list: " ++ show other)

parseFactor :: Arguments -> [Token] -> (AST, [Token])
parseFactor _ [] = error "expected (, string or integer, but got end of input"
parseFactor args (t : ts) = case t of
  TInt num -> (Imm num, ts)
  TStr arg -> (safeLookup arg, ts)
  TChar '(' -> (fst $ parseExpression args (takeParens (t : ts)), skipParens (t : ts))
  other -> error ("expected (, string or integer, but got " ++ show other)
  where
    safeLookup arg = case M.lookup arg args of
      Just num -> Arg num
      Nothing -> error ("invalid argument: " ++ arg)

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
    go [] _ = error "expected ), but got end of input"

takeParens :: [Token] -> [Token]
takeParens tokens = reverse $ go tokens 0 []
  where
    go :: [Token] -> Int -> [Token] -> [Token]
    go (t : ts) 0 [] = case t of
      TChar '(' -> go ts 1 []
      other -> error ("expected (, but got " ++ show other)
    go (t : ts) 1 res = case t of
      TChar '(' -> go ts 2 (TChar '(' : res)
      TChar ')' -> res
      other -> go ts 1 (other : res)
    go (t : ts) n res = case t of
      TChar '(' -> go ts (n + 1) (TChar '(' : res)
      TChar ')' -> go ts (n - 1) (TChar ')' : res)
      other -> go ts n (other : res)
    go [] _ _ = error "the number of opening and closing parentheses don't match"

parseTerm :: Arguments -> [Token] -> (AST, [Token])
parseTerm args tokens =
  let (f1, tokens1) = parseFactor args tokens
   in go f1 tokens1
  where
    go acc (op : tokens2)
      | op == TChar '*' =
          let (f2, tokens3) = parseFactor args tokens2
           in go (Mul acc f2) tokens3
    go acc (op : tokens2)
      | op == TChar '/' =
          let (f2, tokens3) = parseFactor args tokens2
           in go (Div acc f2) tokens3
    go acc rest = (acc, rest)

parseExpression :: Arguments -> [Token] -> (AST, [Token])
parseExpression args tokens =
  let (f1, tokens1) = parseTerm args tokens
   in go f1 tokens1
  where
    go acc (op : tokens2)
      | op == TChar '+' =
          let (f2, tokens3) = parseTerm args tokens2
           in go (Add acc f2) tokens3
    go acc (op : tokens2)
      | op == TChar '-' =
          let (f2, tokens3) = parseTerm args tokens2
           in go (Sub acc f2) tokens3
    go acc rest = (acc, rest)

parse :: [Token] -> AST
parse tokens = case nonEmpty tokens of
  Nothing -> error "input is empty"
  Just ts -> parse' ts

parse' :: NonEmpty Token -> AST
parse' tokens = fst $ parseExpression args tokens1
  where
    args' = parseArgs tokens
    args = fst args'
    tokens1 = snd args'
