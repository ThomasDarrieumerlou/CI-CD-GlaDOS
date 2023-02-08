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
import AstTests (astTestList)

main :: IO ()
main = runTestTT ( test [
    astTestList, cptToAstTestList, cptTestList, evaluationTestList, operatorTestList,
    literalTestList, lexerTestList])