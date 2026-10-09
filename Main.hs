module Main where

import Syntax
import Parser
import Eval
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "MiniLisp Interactive REPL"
  putStrLn "Type :ast <expr> to inspect syntax, or :quit to exit."
  replLoop defaultEnv

replLoop :: Env -> IO ()
replLoop env = do
  putStr "mini> "
  hFlush stdout
  input <- getLine

  case input of
    ":quit" -> putStrLn "Exiting."
    _ | take 5 input == ":ast " -> do
        let code = drop 5 input
        case parse code of
          Left err  -> putStrLn err
          Right ast -> print ast
        replLoop env

    "" -> replLoop env

    _ -> case parse input of
      Left parseErr -> do
        putStrLn parseErr
        replLoop env
      Right ast -> case eval ast env of
        Left evalErr -> do
          putStrLn evalErr
          replLoop env
        Right (result, newEnv) -> do
          print result
          replLoop newEnv