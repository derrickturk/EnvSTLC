module Main where

import Language.EnvSTLC.Repl

main :: IO ()
main = banner >> runRepl_ replLoop initialState

banner :: IO ()
banner = do
  putStrLn "     ________ _       _____  __ ______ ________ __       _______"
  putStrLn "    / ______// \\     / /| | / // ____//___  __// /      / _____/"
  putStrLn "   / /_____ / _ \\   / / | |/ // /___     / /  / /      / /      v0.1"
  putStrLn "  / ______// / \\ \\_/ /  | | //___  /    / /  / /      / /       by dwt @"
  putStrLn " / /_____ / /   \\   /   |  /____/ /    / /  / /_____ / /____    terminus"
  putStrLn "/_______//_/     \\_/    |_//_____/    /_/  /_______//______/    data science LLC"
  putStrLn ""
