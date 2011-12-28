module Trajectory.Private.Config where

import qualified Data.ByteString.Char8 as BS (readFile, ByteString)
import System.Environment (getEnv)
import qualified Data.Map as M (lookup)
import qualified Data.Text as T (pack, unpack)
import Data.Aeson (json, Value(..))
import Data.Attoparsec (parse, Result(..))

withKey profileName doThis = do
  maybeKey <- getKey profileName
  case maybeKey of
    Nothing -> putStrLn $ "Unknown profile name: " ++ profileName
    (Just key) -> doThis key

getKey profileName = do
  possibleJsonString <- readConfig
  case possibleJsonString of
    Nothing -> return Nothing
    (Just jsonString) -> do
      return $ maybe Nothing (Just . extractKey) maybeKey
      where
        extractKey (String k) = T.unpack k
        (Done _ config) = parse json jsonString
        (Object mapping) = config
        maybeKey = M.lookup (T.pack profileName) mapping

readConfig :: IO (Maybe BS.ByteString)
readConfig = do
  configFileName <- getConfigFileName
  (BS.readFile configFileName >>= return . Just) `catch` (const $ return Nothing)

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
