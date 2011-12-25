module Main where

import Text.RegexPR (matchRegexPR)
import System.Environment (getArgs)
import Data.Maybe (isJust, fromMaybe)
import System.IO (hFlush, stdout)

-- this is all used for writeKey:
import Trajectory.Private

main = do
  args <- getArgs
  (config, specificArgs) <- getConfig args
  key <- getKey
  writeKey config key
  return ()

getKey = promptWhile isBlank "API key: " 

writeKey config key =
  let configUpdater = getConfigUpdater config
      updatedConfig = configUpdater key in
    getConfigWriter updatedConfig

-- generally useful functions below; maybe they exist elsewhere:

promptWhile p prompt = loop
  where
    loop = do
      putStr prompt
      hFlush stdout
      result <- getLine
      if p result
         then loop
         else return result

isBlank s = isJust $ matchRegexPR "^\\s*$" s
