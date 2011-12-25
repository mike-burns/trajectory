module Trajectory.Private where

import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnv)
import Data.Aeson (json, Value(..))
import Data.Attoparsec (parse, Result(..))

getConfig = do
  configFileName <- getConfigFileName
  jsonString <- BS.readFile configFileName
  let (Done _ config) = parse json jsonString
      (Object mapping) = config
  return mapping

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
