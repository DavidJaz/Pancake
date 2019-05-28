module Main where

import Parse (readLine)
import Cmds
import Machine (repl, load, pretty_print)

import Flow
import Text.Parsec
import System.IO
import System.Directory

main =
  do
    putStrLn "Libary File: "
    lib <- getLine
    fileExists <- doesFileExist lib
    if fileExists
      then do
        imports <- fmap readLine $ readFile lib
        repl imports
      else do
        putStrLn "File not found"
        imports <- return $ Right []
        repl imports
