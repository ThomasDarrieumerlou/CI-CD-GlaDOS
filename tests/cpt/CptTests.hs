module CptTests (cptTestList) where

import Test.HUnit

import Cpt (Cpt (Integer, Symbol, List), getSymbol, getInteger, getList)

-- -------------------------------------------------------------------------- --
--                                  Test list                                 --
-- -------------------------------------------------------------------------- --

cptTestList :: Test
cptTestList = TestList [ getSymbolNormal, getSymbolInt, getSymbolList, getIntegerInt, getIntegerSymbol, getIntegerList]

-- -------------------------------------------------------------------------- --
--                               getSymbol Tests                              --
-- -------------------------------------------------------------------------- --

getSymbolNormal :: Test
getSymbolNormal = TestCase (assertEqual "For getSymbol \"s\""
    (Just "s")
    (getSymbol (Symbol "s"))
    )

getSymbolInt :: Test
getSymbolInt = TestCase (assertEqual "For getSymbol Int"
    Nothing
    (getSymbol (Integer 1))
    )

getSymbolList :: Test
getSymbolList = TestCase (assertEqual "For getSymbol list"
    Nothing
    (getSymbol (List [Symbol "s"]))
    )

-- -------------------------------------------------------------------------- --
--                              getInteger Tests                              --
-- -------------------------------------------------------------------------- --

getIntegerInt :: Test
getIntegerInt = TestCase (assertEqual "For getInteger 1"
    (Just 1)
    (getInteger (Integer 1))
    )

getIntegerSymbol :: Test
getIntegerSymbol = TestCase (assertEqual "For getInteger symbol"
    Nothing
    (getInteger (Symbol "a"))
    )

getIntegerList :: Test
getIntegerList = TestCase (assertEqual "For getInteger [\"s\"]"
    Nothing
    (getInteger (List [Symbol "s"]))
    )

-- -------------------------------------------------------------------------- --
--                                getList Tests                               --
-- -------------------------------------------------------------------------- --

-- getListList :: Test
-- getListList = TestCase (assertEqual "For getList [\"s\"]"
--     (Just [Symbol "s"])
--     (getList (List [Symbol "s"]))
--     )

-- getListInt :: Test
-- getListInt = TestCase (assertEqual "For getList 1"
--     Nothing
--     (getList (Integer 1))
--     )

-- getListSymbol :: Test
-- getListSymbol = TestCase (assertEqual "For getList symbol"
--     Nothing
--     (getList (Symbol "a"))
--     )
