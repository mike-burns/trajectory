module Main where

import Text.RegexPR (matchRegexPR)
import System.Environment (getArgs)
import Data.Maybe (isJust, fromMaybe)
import System.IO (hFlush, stdout)

-- this is all used for writeKey:
import Trajectory.Private
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (encode, toJSON, Value(..))

main = do
  args <- getArgs
  let profileName = getProfileNameFrom args
  configFileName <- getConfigFileName
  key <- getKey
  writeKey configFileName profileName key
  return ()

getProfileNameFrom args =
  fromMaybe "default" $ args `elementAfter` "--profile"

getKey = promptWhile isBlank "API key: " 

writeKey configFileName profileName key = do
  configFileName <- getConfigFileName
  priorConfig <- getConfig
  let config = M.insert (T.pack profileName) (String $ T.pack key) priorConfig
      json = toJSON config
  BS.writeFile configFileName $ encode json

-- generally useful functions below; maybe they exist elsewhere:

elementAfter [] _ = Nothing
elementAfter (x:xs) match
  | x == match = nextElement xs
  | otherwise = elementAfter xs match
    where
      nextElement [] = Nothing
      nextElement (result:_) = Just result

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
