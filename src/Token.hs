module Token (Token (..), tokenize) where

data Token
  = TChar Char
  | TInt Int
  | TStr String
  deriving (Eq, Show)

alpha :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

digit :: String
digit = ['0' .. '9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c : cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs
