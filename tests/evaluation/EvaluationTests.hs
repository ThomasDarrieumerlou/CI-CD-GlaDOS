{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Operators.hs
-}

module EvaluationTests (evaluationTestList) where

import Test.HUnit
import Prelude hiding (lookup)


import Ast (Ast (..), Operator (..))
import Data.Map (fromList, empty)
import Evaluation (evalAst)
import Literal (Literal (Integer, Inexact, Floating, Boolean))

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

evaluationTestList :: Test
evaluationTestList = TestList [
    callVariableTest, callFunctionTest, callGlobalTest, callOverrideTest,
    defineVariableTest, defineLambdaTest, functionTest,
    simplePlusTest, onePlusTest, noPlusTest, multiPlusTest,
    simpleMinusTest, oneMinusTest, noMinusTest, multiMinusTest,
    simpleMultiplyTest, oneMultiplyTest, noMultiplyTest, multiMultiplyTest,
    simpleDivideTest, oneDivideTest, noDivideTest, multiDivideTest
  ]

-- -------------------------------------------------------------------------- --
--                                  Call Tests                                --
-- -------------------------------------------------------------------------- --

callVariableTest :: Test
callVariableTest = TestCase (assertEqual "For a simple variable call"
    (Just (Value (Integer 4)))
    (fst $ evalAst (Call "x" []) (fromList [("x", Value (Integer 4))]))
  )

callFunctionTest :: Test
callFunctionTest = TestCase (assertEqual "For a simple function call"
    (Just (Value (Integer 7)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 4)]) (fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))
    ]))
  )

callGlobalTest :: Test
callGlobalTest = TestCase (assertEqual "For a global variable call in a func"
    (Just (Value (Integer 11)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 4)]) (fromList [
      ("f", Function ["a", "b"]
        (Operator Plus [Call "a" [], Call "b" [], Call "x" []])),
      ("x", Value (Integer 4))
    ]))
  )

callOverrideTest :: Test
callOverrideTest = TestCase (assertEqual "For a parameter that overrides a var"
    (Just (Value (Integer 5)))
    (fst $ evalAst (Call "f" [Value (Integer 3), Value (Integer 2)])
    (fromList [
      ("f", Function ["a", "b"] (Operator Plus [Call "a" [],
        Call "b" []])),
      ("a", Value (Integer 4))
    ]))
  )

-- -------------------------------------------------------------------------- --
--                                Define tests                                --
-- -------------------------------------------------------------------------- --

defineVariableTest :: Test
defineVariableTest = TestCase (assertEqual "For a simple variable definition"
    (fromList [("x", Value (Integer 4))])
    (snd $ evalAst (Define "x" (Value (Integer 4))) empty)
  )

defineLambdaTest :: Test
defineLambdaTest = TestCase (assertEqual "For a simple function definition"
    (fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))])
    (snd $ evalAst (Define "f" (Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))) empty)
  )


-- -------------------------------------------------------------------------- --
--                               Function tests                               --
-- -------------------------------------------------------------------------- --

functionTest :: Test
functionTest = TestCase (assertEqual "For a simple function definition"
    (fromList [("f", Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))])
    (snd $ evalAst (Define "f" (Function ["a", "b"]
      (Operator Plus [Call "a" [], Call "b" []]))) empty)
  )

-- -------------------------------------------------------------------------- --
--                               Operator tests                               --
-- -------------------------------------------------------------------------- --

simplePlusTest :: Test
simplePlusTest = TestCase (assertEqual "For a simple plus"
    (Just (Value (Integer 7)))
    (fst $ evalAst (Operator Plus [Value (Integer 3), Value (Integer 4)]) empty)
  )

onePlusTest :: Test
onePlusTest = TestCase (assertEqual "For a plus with one argument"
    (Just (Value (Integer 3)))
    (fst $ evalAst (Operator Plus [Value (Integer 3)]) empty)
  )

multiPlusTest :: Test
multiPlusTest = TestCase (assertEqual "For a plus with multiple arguments"
    (Just (Value (Integer 10)))
    (fst $ evalAst (Operator Plus [Value (Integer 3), Value (Integer 4), Value (Integer 3)]) empty)
  )

noPlusTest :: Test
noPlusTest = TestCase (assertEqual "For a plus with no argument"
    (Just (Value (Integer 0)))
    (fst $ evalAst (Operator Plus []) empty)
  )

simpleMinusTest :: Test
simpleMinusTest = TestCase (assertEqual "For a simple minus"
    (Just (Value (Integer 1)))
    (fst $ evalAst (Operator Minus [Value (Integer 4), Value (Integer 3)]) empty)
  )

oneMinusTest :: Test
oneMinusTest = TestCase (assertEqual "For a minus with one argument"
    (Just (Value (Integer (-3))))
    (fst $ evalAst (Operator Minus [Value (Integer 3)]) empty)
  )

multiMinusTest :: Test
multiMinusTest = TestCase (assertEqual "For a minus with multiple argument"
    (Just (Value (Integer 0)))
    (fst $ evalAst (Operator Minus [Value (Integer 10), Value (Integer 4), Value (Integer 3), Value (Integer 3)]) empty)
  )

noMinusTest :: Test
noMinusTest = TestCase (assertEqual "For a minus with no argument"
    Nothing
    (fst $ evalAst (Operator Minus []) empty)
  )

simpleMultiplyTest :: Test
simpleMultiplyTest = TestCase (assertEqual "For a simple multiply"
    (Just (Value (Integer 12)))
    (fst $ evalAst (Operator Times [Value (Integer 4), Value (Integer 3)]) empty)
  )

oneMultiplyTest :: Test
oneMultiplyTest = TestCase (assertEqual "For a multiply with one argument"
    (Just (Value (Integer 3)))
    (fst $ evalAst (Operator Times [Value (Integer 3)]) empty)
  )

noMultiplyTest :: Test
noMultiplyTest = TestCase (assertEqual "For a simple multiply"
    (Just (Value (Integer 1)))
    (fst $ evalAst (Operator Times []) empty)
  )

multiMultiplyTest :: Test
multiMultiplyTest = TestCase (assertEqual "For a multiply with multiple parameters"
    (Just (Value (Integer 24)))
    (fst $ evalAst (Operator Times [Value (Integer 4), Value (Integer 3), Value (Integer 2)]) empty)
  )

simpleDivideTest :: Test
simpleDivideTest = TestCase (assertEqual "For a simple divide"
    (Just (Value (Integer 2)))
    (fst $ evalAst (Operator Div [Value (Integer 4), Value (Integer 2)]) empty)
  )

oneDivideTest :: Test
oneDivideTest = TestCase (assertEqual "For a divide with one argument"
    (Just (Value (Inexact 1 3)))
    (fst $ evalAst (Operator Div [Value (Integer 3)]) empty)
  )

noDivideTest :: Test
noDivideTest = TestCase (assertEqual "For a divide with no argument"
    Nothing
    (fst $ evalAst (Operator Div []) empty)
  )

multiDivideTest :: Test
multiDivideTest = TestCase (assertEqual "For a divide with multiple parameters"
    (Just (Value (Integer 5)))
    (fst $ evalAst (Operator Div [Value (Integer 20), Value (Integer 2), Value (Integer 2)]) empty)
  )