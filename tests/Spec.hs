{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Spec.hs
-}

import Test.HUnit

import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))

import CptToAst (cptToAstTestList)
import CptTests (cptTestList)
import Lexer (lexerTestList)
import EvaluationOperatorTests (operatorTestList)
import EvaluationTests (evaluationTestList)
import LiteralTests (literalTestList)

main :: IO ()
main = runTestTT ( test [
    cptToAstTestList, cptTestList, evaluationTestList, operatorTestList, literalTestList,
    lexerTestList
  ]) >>= (\x -> if errors x + failures x == 0
    then  exitSuccess
    else exitWith (ExitFailure 84))
