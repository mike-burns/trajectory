module Trajectory.Private where

import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnv)
import Data.Aeson (json, Value(..))
import Data.Attoparsec (parse, Result(..))

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (encode, toJSON, Value(..))
import Data.Maybe (fromMaybe, fromJust)

data Config = Config {
   getConfigKey :: String
  ,getConfigUpdater :: String -> Config
  ,getConfigWriter :: IO ()
}

mkConfig map args =
  Config {
    getConfigKey =
      let (String key) = fromJust $ M.lookup (T.pack profileName) map in
        T.unpack key
   ,getConfigUpdater = \newKey ->
     let newMap = M.insert (T.pack profileName) (String $ T.pack newKey) map in
       mkConfig newMap args
   ,getConfigWriter = do
     configFileName <- getConfigFileName
     LBS.writeFile configFileName $ encode $ toJSON map
  }
  where
    profileName = fromMaybe "default" $ args `elementAfter` "--profile"

getConfig args = do
  configFileName <- getConfigFileName
  jsonString <- BS.readFile configFileName
  let (Done _ config) = parse json jsonString
      (Object mapping) = config
  return $ (mkConfig mapping args, args)

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

-- this is pretty generic:

elementAfter [] _ = Nothing
elementAfter (x:xs) match
  | x == match = nextElement xs
  | otherwise = elementAfter xs match
    where
      nextElement [] = Nothing
      nextElement (result:_) = Just result
