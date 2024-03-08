module Parser (initParser, parse) where

import AST
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Token

precedence :: Token -> Int
precedence ast = case ast of
  TChar '+' -> 0
  TChar '-' -> 0
  TChar '*' -> 1
  TChar '/' -> 1
  _notOperator -> undefined

parse :: [Token] -> AST
parse = ast . parse' . initParser

skipParens :: [Token] -> [Token]
skipParens p = skipParens' p 0

skipParens' :: [Token] -> Int -> [Token]
skipParens' p c = case p of
  [] -> p
  (x : xs) -> case x of
    TChar '(' -> skipParens' xs (c + 1)
    TChar ')' | c > 1 -> skipParens' xs (c - 1)
    TChar ')' | c == 1 -> xs
    _other -> skipParens' xs c

parseInfix :: Parser -> Parser
parseInfix p = case nextOp of
  Just nOp ->
    if precedence op < precedence nOp
      then parseLowPrecedence p
      else parseHighPrecedence p
  Nothing -> parseHighPrecedence p
  where
    op = head $ tokens p
    nextOp = case tail $ tokens p of
      [] -> Nothing
      ((TChar '(') : xs) -> case skipParens (TChar '(' : xs) of
        [] -> Nothing
        (x : _) -> Just x
      (_ : xs) -> case xs of
        [] -> Nothing
        (y : _) | y == TChar ')' -> Nothing
        (y : _) -> Just y
    parseLowPrecedence p' =
      let newP = parse' $ p' {ast = ASTUndefined, tokens = tail $ tokens p'}
       in let newAst = case op of
                TChar '+' -> Add (ast p') (ast newP)
                TChar '-' -> Sub (ast p') (ast newP)
                TChar '*' -> Mul (ast p') (ast newP)
                TChar '/' -> Div (ast p') (ast newP)
                _other -> undefined
           in parse' p' {ast = newAst, tokens = tokens newP}
    parseHighPrecedence p' =
      let newP = case tail $ tokens p' of
            [] -> undefined
            (TChar '(' : xs) -> parse' $ p' {ast = ASTUndefined, tokens = xs}
            (x : xs) -> (parse' $ p' {ast = ASTUndefined, tokens = [x]}) {tokens = xs}
       in let newAst = case op of
                TChar '+' -> Add (ast p') (ast newP)
                TChar '-' -> Sub (ast p') (ast newP)
                TChar '*' -> Mul (ast p') (ast newP)
                TChar '/' -> Div (ast p') (ast newP)
                _other -> undefined
           in parse' p' {ast = newAst, tokens = tokens newP}

parse' :: Parser -> Parser
parse' p = case tokens p of
  [] -> p
  (t : ts) -> case state p of
    PInvalid -> p {tokens = []}
    PDefault -> case t of
      TChar '[' -> parse' $ p {tokens = ts, state = PArgs}
      _invalid -> parse' $ p {tokens = [], state = PInvalid}
    PArgs -> case t of
      TStr str -> parse' $ p {tokens = ts, idx = idx p + 1, args = M.insert str (idx p) (args p)}
      TChar ']' -> parse' $ p {tokens = ts, state = PFunction}
      _invalid -> parse' $ p {tokens = [], state = PInvalid}
    PFunction -> case t of
      TChar '(' -> parse' $ p {tokens = tokens ip, ast = ast ip}
        where
          ip = parse' p {ast = ASTUndefined, tokens = ts}
      TChar ')' -> p {tokens = ts}
      TChar '+' -> parseInfix p
      TChar '-' -> parseInfix p
      TChar '*' -> parseInfix p
      TChar '/' -> parseInfix p
      TChar _ -> p {state = PInvalid}
      TInt num -> case ast p of
        ASTUndefined -> parse' $ p {tokens = ts, ast = Imm num}
        Add ASTUndefined other -> parse' $ p {tokens = ts, ast = Add (Imm num) other}
        Add other ASTUndefined -> parse' $ p {tokens = ts, ast = Add other (Imm num)}
        Sub ASTUndefined other -> parse' $ p {tokens = ts, ast = Sub (Imm num) other}
        Sub other ASTUndefined -> parse' $ p {tokens = ts, ast = Sub other (Imm num)}
        Mul ASTUndefined other -> parse' $ p {tokens = ts, ast = Mul (Imm num) other}
        Mul other ASTUndefined -> parse' $ p {tokens = ts, ast = Mul other (Imm num)}
        Div ASTUndefined other -> parse' $ p {tokens = ts, ast = Div (Imm num) other}
        Div other ASTUndefined -> parse' $ p {tokens = ts, ast = Div other (Imm num)}
        _invalid -> p {tokens = [], state = PInvalid}
      TStr str -> case ast p of
        ASTUndefined -> parse' $ p {tokens = ts, ast = Arg $ fromJust $ M.lookup str $ args p}
        Add ASTUndefined other -> parse' $ p {tokens = ts, ast = Add (Arg $ fromJust $ M.lookup str $ args p) other}
        Add other ASTUndefined -> parse' $ p {tokens = ts, ast = Add other (Arg $ fromJust $ M.lookup str $ args p)}
        Sub ASTUndefined other -> parse' $ p {tokens = ts, ast = Sub (Arg $ fromJust $ M.lookup str $ args p) other}
        Sub other ASTUndefined -> parse' $ p {tokens = ts, ast = Sub other (Arg $ fromJust $ M.lookup str $ args p)}
        Mul ASTUndefined other -> parse' $ p {tokens = ts, ast = Mul (Arg $ fromJust $ M.lookup str $ args p) other}
        Mul other ASTUndefined -> parse' $ p {tokens = ts, ast = Mul other (Arg $ fromJust $ M.lookup str $ args p)}
        Div ASTUndefined other -> parse' $ p {tokens = ts, ast = Div (Arg $ fromJust $ M.lookup str $ args p) other}
        Div other ASTUndefined -> parse' $ p {tokens = ts, ast = Div other (Arg $ fromJust $ M.lookup str $ args p)}
        _invalid -> p {tokens = [], state = PInvalid}

initParser :: [Token] -> Parser
initParser tokens =
  Parser
    { ast = ASTUndefined,
      tokens = tokens,
      args = M.empty,
      state = PDefault,
      idx = 0
    }

data Parser = Parser
  { ast :: AST,
    tokens :: [Token],
    args :: M.Map String Int,
    state :: PState,
    idx :: Int
  }
  deriving (Show, Eq)

data PState = PInvalid | PDefault | PArgs | PFunction deriving (Show, Eq)
