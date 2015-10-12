module Main where

  import System.Environment
  import TaskList

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      [] -> dispatch "-help" []
      cmd:params -> dispatch cmd params
