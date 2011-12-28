{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Trajectory.Private.API (
 getStories
,module Trajectory.Types
) where

import Data.Data
import Data.Aeson
import Control.Applicative ( (<$>), (<*>) )

import Data.List (intercalate)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as Types
import Network.HTTP.Enumerator
import Text.URI
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)

import Trajectory.Types

data Stories = Stories [Story] deriving (Show, Eq, Typeable, Data)

instance FromJSON Story where
  parseJSON (Object o) =
    Story <$> o .: "archived"
          <*> o .:? "assignee_id"
          <*> o .:? "branch"
          <*> o .: "created_at"
          <*> o .: "deleted"
          <*> o .: "design_needed"
          <*> o .: "development_needed"
          <*> o .: "id"
          <*> o .:? "idea_id"
          <*> o .: "iteration_id"
          <*> o .: "points"
          <*> o .: "position"
          <*> o .: "state"
          <*> o .: "task_type"
          <*> o .: "title"
          <*> o .: "updated_at"
          <*> o .: "user_id"
          <*> o .: "comments_count"
          <*> o .:? "assignee_name"
          <*> o .: "user_name"
          <*> o .: "state_events"
          <*> o .:? "idea_subject"
  parseJSON _          = fail "Could not build a Story"

instance FromJSON Stories where
  parseJSON (Object o) =
    Stories <$> o .: "stories"
  parseJSON _          = fail "Could not build Stories"

getStories :: String -> String -> String -> IO (Either Error [Story])
getStories key accountName projectName = do
  let url = buildUrl [key, "accounts", accountName, "projects", projectName, "stories.json"]
  result <- doHttps (BS.pack "GET") url Nothing
  return $ either (Left . HTTPConnectionError)
                  (extractStories . parseJson . responseBody)
                  result
  where
    extractStories :: (Either Error Stories) -> (Either Error [Story])
    extractStories (Left l) = Left l
    extractStories (Right (Stories ss)) = Right ss


buildUrl :: [String] -> String
buildUrl paths = "https://www.apptrajectory.com/api/" ++ intercalate "/" paths


doHttps :: BS.ByteString -> String -> Maybe (RequestBody IO) -> IO (Either E.IOException Response)
doHttps method url body = do
  let (Just uri) = parseURI url
      (Just host) = uriRegName uri
      requestBody = fromMaybe (RequestBodyBS $ BS.pack "") body
      queryString = Types.parseQuery $ BS.pack $ fromMaybe "" $ uriQuery uri
      request = def { method = method
                    , secure = True
                    , host = BS.pack host
                    , port = 443
                    , path = BS.pack $ uriPath uri
                    , requestBody = requestBody
                    , queryString = queryString
                    }

  (getResponse request >>= return . Right) `catch` (return . Left)
  where
    getResponse request = withManager $ \manager -> httpLbs request manager

parseJson :: (FromJSON b, Show b) => LBS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.ByteString.Lazy.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError $ e ++ " on the JSON: " ++ LBS.unpack jsonString
       (Fail _ _ e) -> Left $ ParseError e
