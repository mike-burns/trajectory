{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Data.Data

import Text.RegexPR (matchRegexPR)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)

import Trajectory.Private.Config (writeKey)

main = do
  args <- cmdArgs initTjArgDefinition
  key <- getKey
  writeKey (profileName args) key
  return ()

getKey = promptWhile isBlank "API key: " 

data InitTjArg = InitTjArg {
   profileName :: String
} deriving (Show, Data, Typeable)

initTjArgDefinition = InitTjArg {
   profileName = "default"
     &= explicit
     &= name "profile"
     &= help "The profile name to use [default]"
} &= program "initrj"

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
