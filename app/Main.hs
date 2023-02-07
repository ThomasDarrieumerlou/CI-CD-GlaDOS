{-
-- EPITECH PROJECT, 2022
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Main.hs
-}

module Main (main) where
import Lexer (pLisp)
import System.Exit

prompt :: IO ()
prompt = putStr "> "

launchCmd :: String -> IO ()
launchCmd "quit" = exitSuccess 
launchCmd str = prompt >> putStrLn str

loop :: IO ()
loop = getLine >>= \line -> launchCmd line >> loop

main :: IO ()
main = loop 
