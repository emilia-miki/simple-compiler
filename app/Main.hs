module Main where

import AST
import Token (tokenize)
import Parser (parse)
import ASTReducer (astReduce)
import Assembler (assemble)

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = parse . tokenize

pass2 :: AST -> AST
pass2 = astReduce

pass3 :: AST -> [String]
pass3 = assemble

main :: IO ()
main = interact (show . pass1)

-- simulate :: [String] -> [Int] -> Int
-- simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
--   step (r0,r1,stack) ins =
--     case ins of
--       ('I':'M':xs) -> (read xs, r1, stack)
--       ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
--       "SW" -> (r1, r0, stack)
--       "PU" -> (r0, r1, r0:stack)
--       "PO" -> (head stack, r1, tail stack)
--       "AD" -> (r0 + r1, r1, stack)
--       "SU" -> (r0 - r1, r1, stack)
--       "MU" -> (r0 * r1, r1, stack)
--       "DI" -> (r0 `div` r1, r1, stack)
--   takeR0 (r0,_,_) = r0
