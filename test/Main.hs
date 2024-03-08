module Main (main) where

import AST (AST (..))
import ASTReducer (astReduce)
import Assembler (Instruction (..), assemble)
import Parser (parse)
import Test.HUnit
import Token (tokenize)

newParserTest :: String -> AST -> Test
newParserTest input expected = TestLabel input $ TestCase (assertEqual ("Input: " ++ input) expected (parse $ tokenize input))

parserTests :: [Test]
parserTests =
  [ newParserTest "[] 1" (Imm 1),
    newParserTest "[] 1 - 2" (Sub (Imm 1) (Imm 2)),
    newParserTest "[ xx yy ] ( xx + yy ) / 2" (Div (Add (Arg 0) (Arg 1)) (Imm 2)),
    newParserTest "[ x ] x + 2 * 5" (Add (Arg 0) (Mul (Imm 2) (Imm 5))),
    newParserTest "[ a b ] a * a + b * b" (Add (Mul (Arg 0) (Arg 0)) (Mul (Arg 1) (Arg 1))),
    newParserTest "[ a b ] a + (a + b) / a" (Add (Arg 0) (Div (Add (Arg 0) (Arg 1)) (Arg 0))),
    newParserTest "[ x y z ] x + (x * y) + z" (Add (Add (Arg 0) (Mul (Arg 0) (Arg 1))) (Arg 2)),
    newParserTest "[ x y z ] x / (x + y) * z" (Mul (Div (Arg 0) (Add (Arg 0) (Arg 1))) (Arg 2))
  ]

newASTReducerTest :: AST -> AST -> Test
newASTReducerTest input expected =
  TestLabel (show input) $ TestCase (assertEqual ("Input: " ++ show input) expected (astReduce input))

astReducerTests :: [Test]
astReducerTests =
  [ newASTReducerTest (Add (Arg 0) (Mul (Imm 2) (Imm 5))) (Add (Arg 0) (Imm 10)),
    newASTReducerTest (Sub (Imm 10) (Imm 2)) (Imm 8),
    newASTReducerTest (Div (Imm 10) (Imm 2)) (Imm 5)
  ]

newAssemblerTest :: AST -> [Instruction] -> Test
newAssemblerTest input expected =
  TestLabel (show input) $ TestCase (assertEqual ("Input: " ++ show input) expected (assemble input))

assemblerTests :: [Test]
assemblerTests =
  [ newAssemblerTest
      (Imm 1)
      [IM 1],
    newAssemblerTest
      (Add (Arg 0) (Imm 10))
      [AR 0, PU, IM 10, SW, PO, AD],
    newAssemblerTest
      (Sub (Imm 1) (Imm 2))
      [IM 1, PU, IM 2, SW, PO, SU],
    newAssemblerTest
      (Div (Add (Arg 0) (Arg 1)) (Imm 2))
      [AR 0, PU, AR 1, SW, PO, AD, PU, IM 2, SW, PO, DI],
    newAssemblerTest
      (Add (Arg 0) (Mul (Imm 2) (Imm 5)))
      [AR 0, PU, IM 2, PU, IM 5, SW, PO, MU, SW, PO, AD],
    newAssemblerTest
      (Add (Arg 0) (Div (Add (Arg 0) (Arg 1)) (Arg 0)))
      [AR 0, PU, AR 0, PU, AR 1, SW, PO, AD, PU, AR 0, SW, PO, DI, SW, PO, AD],
    newAssemblerTest
      (Add (Add (Arg 0) (Mul (Arg 0) (Arg 1))) (Arg 2))
      [AR 0, PU, AR 0, PU, AR 1, SW, PO, MU, SW, PO, AD, PU, AR 2, SW, PO, AD],
    newAssemblerTest
      (Mul (Div (Arg 0) (Add (Arg 0) (Arg 1))) (Arg 2))
      [AR 0, PU, AR 0, PU, AR 1, SW, PO, AD, SW, PO, DI, PU, AR 2, SW, PO, MU]
  ]

tests :: Test
tests = TestList (parserTests ++ astReducerTests ++ assemblerTests)

main :: IO ()
main = runTestTTAndExit tests
