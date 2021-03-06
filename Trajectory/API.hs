{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | The Trajectory API, or a subset of it at least. This mirrors the
-- underlying implementation, which ties stories to iterations.

module Trajectory.API (
 getStories
,module Trajectory.Types
) where

import Data.Data
import Data.Aeson
import Control.Applicative ( (<$>), (<*>), pure )

import Data.List (intercalate)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as Types
import Network.HTTP.Enumerator
import Text.URI
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import Data.Time (parseTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T (unpack)

import Trajectory.Types

-- | Get all the incomplete stories and iterations for a given user key,
-- account name, and project name. Since stories and iterations are tied
-- together in the underlying API, this produces them as a pair.
--
-- It produces an IO of either an error or the stories/iterations pair. The
-- error can come from the HTTP, or from non-JSON input, or from a change to
-- the JSON.
--
-- > do
-- >   possibleStories <- getStories "abcdefg" "thoughtbot" "opensource"
-- >   case possibleStories of
-- >     (Left error) -> putStrLn $ "got the error: " ++ show error
-- >     (Right (stories,iterations)) ->
-- >       putStrLn $ intercalate "\n" $
-- >         (map formatStory stories) ++ (map formatIteration iterations)
getStories :: String -> String -> String -> IO (Either Error ([Story], [Iteration]))
getStories key accountName projectName = do
  let url = buildUrl [key, "accounts", accountName, "projects", projectName, "stories.json"]
  result <- doHttps (BS.pack "GET") url Nothing
  return $ either (Left . HTTPConnectionError)
                  (extractStories . parseJson . responseBody)
                  result
  where
    extractStories :: (Either Error Stories) -> (Either Error ([Story],[Iteration]))
    extractStories (Left l) = Left l
    extractStories (Right (Stories stories iterations)) = Right (stories, iterations)


data Stories = Stories [Story] [Iteration]
  deriving (Show, Eq, Typeable, Data)

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

instance FromJSON Iteration where
  parseJSON (Object o) =
    Iteration <$> o .: "accepted_points"
              <*> o .: "complete"
              <*> o .: "created_at"
              <*> o .: "estimated_points"
              <*> o .: "estimated_velocity"
              <*> o .: "id"
              <*> o .: "starts_on"
              <*> o .: "stories_count"
              <*> o .: "team_strength"
              <*> o .: "updated_at"
              <*> o .: "percent_complete"
              <*> o .: "current?"
              <*> o .: "unstarted_stories_count"
              <*> o .: "accepted_stories_count"
              <*> o .: "started_stories_count"
              <*> o .: "delivered_stories_count"
              <*> o .: "comments_count"
  parseJSON _ = fail "Could not build an Iteration"

instance FromJSON Stories where
  parseJSON (Object o) =
    Stories <$> o .: "stories" <*> o .: "iterations"
  parseJSON _          = fail "Could not build Stories"

instance FromJSON TimeWithZone where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%FT%T%Z" (T.unpack t) of
         Just d -> pure $ TimeWithZone d
         _      -> fail "could not parse a Rails default datetime"
  parseJSON _          = fail "Given something besides a String to parse into a datetime"


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
