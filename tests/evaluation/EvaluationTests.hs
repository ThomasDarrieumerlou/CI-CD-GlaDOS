{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module EvaluationTests (evaluationTestList) where

import Test.HUnit

import Ast (Ast (..), Operator (..))
import Data.Map (fromList)
import Evaluation (evalAst)
import Literal (Literal (Integer, Inexact, Floating, Boolean))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

evaluationTestList :: Test
evaluationTestList = TestList [
    callVariableTest, callFunctionTest, callGlobalTest, callOverrideTest
  ]

-- -------------------------------------------------------------------------- --
--                                  Call Tests                                --
-- -------------------------------------------------------------------------- --

callVariableTest :: Test
callVariableTest = TestCase (assertEqual "For a simple variable call"
    (Just (Value (Integer 4)))
    (fst $ evalAst (Call "x" []) (fromList [("x", (Value (Integer 4)))]))
  )

callFunctionTest :: Test
callFunctionTest = TestCase (assertEqual "For a simple function call"
    (Just (Value (Integer 7)))
    (fst $ evalAst (Call "f" [(Value (Integer 3)), (Value (Integer 4))]) (fromList [("f", (Function ["a", "b"]
      (Operator Plus [(Call "a" []), (Call "b" [])]))
    )]))
  )

callGlobalTest :: Test
callGlobalTest = TestCase (assertEqual "For a global variable call in a func"
    (Just (Value (Integer 11)))
    (fst $ evalAst (Call "f" [(Value (Integer 3)), (Value (Integer 4))]) (fromList [
      ("f", (Function ["a", "b"]
        (Operator Plus [(Call "a" []), (Call "b" []), (Call "x" [])]))),
      ("x", (Value (Integer 4)))
    ]))
  )

callOverrideTest :: Test
callOverrideTest = TestCase (assertEqual "For a parameter that overrides a var"
    (Just (Value (Integer 5)))
    (fst $ evalAst (Call "f" [(Value (Integer 3)), (Value (Integer 2))])
    (fromList [
      ("f", (Function ["a", "b"] (Operator Plus [(Call "a" []),
        (Call "b" [])]))),
      ("a", (Value (Integer 4)))
    ]))
  )