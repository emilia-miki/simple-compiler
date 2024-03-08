module Main where

import AST (AST)
import ASTReducer (astReduce)
import Assembler (Instruction, assemble)
import Parser (parse)
import Token (tokenize)

compile :: String -> String
compile = unlines . map show . pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = parse . tokenize

pass2 :: AST -> AST
pass2 = astReduce

pass3 :: AST -> [Instruction]
pass3 = assemble

main :: IO ()
main = interact compile
