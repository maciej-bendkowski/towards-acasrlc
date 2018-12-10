module Main where

import System.Environment

import Grammar
import Mathematica

main :: IO ()
main = do
    args <- getArgs
    let n = read (head args) :: Int
    putStrLn $ toMathematica n
