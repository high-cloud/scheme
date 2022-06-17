{-# LANGUAGE OverloadedStrings #-}

module Repl (
  mainLoop,
) where

import Eval ( safeExec, evalText , evalText', stdenv)
--import Eval ( runParseTest )
import Data.Text as T ( pack )

import Control.Monad.Trans ( MonadIO(liftIO) )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import LispVal (EnvCtx)

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = 
  -- runInputT defaultSettings repl
  do
    env <- stdenv
    runInputT defaultSettings $ repl' env

repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl
    --Just input -> (liftIO $ processToAST input) >> repl


repl' :: EnvCtx -> Repl ()
repl' env = do
  minput <- getInputLine "Repl> "
  let cur = return env
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process' input cur) >>= repl'

process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

process' :: String -> IO EnvCtx -> IO EnvCtx
process' str old= do
  env <- old
  res <- safeExec $ evalText'  (T.pack str) env
  case res of
    Left errorStr ->do
      putStrLn errorStr
      return env
    Right env' -> return env'

--processToAST :: String -> IO ()
--processToAST str = print $ runParseTest $ T.pack str
