module Trajectory.Private.Config where

import qualified Data.ByteString.Char8 as BS (readFile, ByteString)
import qualified Data.ByteString.Lazy as LBS
import System.Environment (getEnv)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T (pack, unpack)
import Data.Aeson (json, Value(..), encode, toJSON)
import Data.Attoparsec (parse, IResult(..))
import Data.Text as T

withKey :: String -> (String -> IO ()) -> IO ()
withKey profileName doThis = do
  maybeKey <- getKey profileName
  maybe showError doThis maybeKey
  where showError = putStrLn $ "Unknown profile name: " ++ profileName

getKey profileName = do
  possibleConfig <- decodeConfig
  return $ maybe Nothing
                 (maybe Nothing (Just . extractKey) . lookupKey)
                 possibleConfig
  where
    extractKey (String k) = T.unpack k
    lookupKey = M.lookup (T.pack profileName)

readConfig :: IO (Maybe BS.ByteString)
readConfig = getConfigFileName >>= readConfigFrom

readConfigFrom :: String -> IO (Maybe BS.ByteString)
readConfigFrom configFileName =
  (BS.readFile configFileName >>= return . Just) `catch` (const $ return Nothing)

decodeConfig :: IO (Maybe (M.HashMap T.Text Value))
decodeConfig = getConfigFileName >>= decodeConfigFrom

decodeConfigFrom :: String -> IO (Maybe (M.HashMap T.Text Value))
decodeConfigFrom configFileName = do
  possibleJsonString <- readConfigFrom configFileName
  case possibleJsonString of
    Nothing -> return Nothing
    (Just jsonString) -> do
      return $ Just mapping
      where
        (Done _ config) = parse json jsonString
        (Object mapping) = config

writeKey :: String -> String -> IO ()
writeKey profileName key = do
  configFileName <- getConfigFileName
  possiblePriorConfig <- decodeConfigFrom configFileName
  let newConfig = maybe (M.singleton encodedProfileName encodedKey)
                        (M.insert encodedProfileName encodedKey)
                        possiblePriorConfig
  LBS.writeFile configFileName $ encode $ toJSON newConfig
  where
    encodedProfileName = T.pack profileName
    encodedKey = String $ T.pack key

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
