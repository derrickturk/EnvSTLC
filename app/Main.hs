module Main where

import Language.EnvSTLC.Repl

main :: IO ()
main = runRepl_ replLoop initialState
