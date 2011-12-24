module Main where

import Text.RegexPR (matchRegexPR)
import System.Environment (getArgs, getEnv)
import Data.Maybe (isJust, fromMaybe)
import System.IO (hFlush, stdout)
import Data.Object.Json (encodeFile, decodeFile)
import Data.Object (Object(..))

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

getConfigFileName = do
  getEnv "TRAJECTORY_CONFIG_FILE"
   `catch`
   (const (getEnv "HOME" >>= return . withHomeDir))
   `catch`
   (const (getEnv "USER" >>= return . withUserName))
   `catch`
   (const (return "/.trajectory"))
  where
    withHomeDir homeDir   = homeDir ++ "/.trajectory"
    withUserName userName = "/usr/"++userName++"/.trajectory"

writeKey configFileName profileName key = do
  priorConfig <- decodeFile configFileName
  let profileYaml = Mapping [(profileName, Scalar key)]
  let yaml = case priorConfig of
               Nothing -> profileYaml
               Just object -> mergeObject object profileYaml
  encodeFile configFileName yaml

-- generally useful functions below; maybe they exist elsewhere:

mergeObject (Mapping m1) (Mapping m2) = Mapping (m2 ++ m1)
mergeObject (Sequence s1) (Sequence s2) = Sequence (s1 ++ s2)
mergeObject (Sequence s) o2@(Scalar v) = Sequence (s ++ [o2])
mergeObject o1@(Scalar v) (Sequence s) = Sequence (o1:s)
mergeObject o1@(Scalar v1) o2@(Scalar v2) = Sequence [o1,o2]
mergeObject o1 o2 = Sequence [o1,o2]

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
