module Main where

import           Data.List
import           Parser
import           RosyBoa
import           System.Environment
import           Text.Printf

main = do
  args <- getArgs
  case args of
       [filename] -> do
         contents <- readFile filename
         runTest contents
       _ -> do
          self <- getProgName
          printf "Usage: ./%s [filename]" self
