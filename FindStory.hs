{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, StandaloneDeriving #-}

module Main where

import System.Environment (getArgs)

import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)

import qualified Control.Exception as E

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Types as Types
import Network.HTTP.Enumerator
import Text.URI

import Data.Aeson
import Data.Attoparsec
import Control.Applicative
import Data.Data

import Trajectory.Private

main = do
  args <- getArgs
  (config, specificArgs) <- getConfig args
  potentiallyAllStories <- (getAllStories config) :: IO (Either Error Stories)
  case potentiallyAllStories of
    (Left error) -> print error
    (Right allStories) ->
      putStrLn $ intercalate "\n\n" $ map formatStory $ getStories allStories

formatStory story =
  title ++ "\n"
    ++ origins ++ "\n"
    ++ overall
  where
    title = storyTitle story
    overall = intercalate "\t" $ [
       "id: " ++ (show $ storyId story)
      ,"points: " ++ (show $ storyPoints story)
      ,"comments: " ++ (show $ storyCommentsCount story)
      ]
    origins = intercalate "\t" $ [
       (fromMaybe "unassigned" $ storyAssigneeName story)
      ,(maybe "free-floating" ("idea: " ++) $ storyIdeaSubject story)
      ]


allStoriesUrl config = do
  let key = getConfigKey config
  return $ "https://www.apptrajectory.com/api/" ++ key ++ "/accounts/923bc9b85eaa4a9213c5/projects/activeblueleaf/stories.json"


deriving instance Eq Network.HTTP.Enumerator.HttpException
data Error =
    HTTPConnectionError E.IOException
  | ParseError String
  | JsonError String
  deriving (Show, Eq)

data Stories = Stories {
   getStories :: [Story]
} deriving (Show, Eq, Typeable, Data)

data Story = Story {
   storyArchived :: Bool
  ,storyAssigneeId :: Maybe Int
  ,storyBranch :: Maybe String
  ,storyCreatedAt :: String
  ,storyDeleted :: Bool
  ,storyDesignNeeded :: Bool
  ,storyDevelopmentNeeded :: Bool
  ,storyId :: Int
  ,storyIdeaId :: Maybe Int
  ,storyIterationId :: Int
  ,storyPoints :: Int
  ,storyPosition :: Int
  ,storyState :: String
  ,storyTaskType :: String
  ,storyTitle :: String
  ,storyUpdatedAt :: String
  ,storyUserId :: Int
  ,storyCommentsCount :: Int
  ,storyAssigneeName :: Maybe String
  ,storyUserName :: String
  ,storyStateEvents :: [String]
  ,storyIdeaSubject :: Maybe String
} deriving (Show, Eq, Typeable, Data)

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

getAllStories :: (FromJSON b, Show b) => Config -> IO (Either Error b)
getAllStories config = do
  url <- allStoriesUrl config
  let method = BS.pack "GET"
      body = Nothing :: Maybe String
      (Just uri)  = parseURI url
      (Just host) = uriRegName uri
      encodedBody = BS.pack $ maybe "" (LBS.unpack . encode . toJSON) body
      queryString = Types.parseQuery $ BS.pack $ maybe "" id $ uriQuery uri
      request = def { method = method
                    , secure = True
                    , host = BS.pack host
                    , port = 443
                    , path = BS.pack $ uriPath uri
                    , requestBody = RequestBodyBS encodedBody
                    , queryString = queryString
                    }

  result <- (getResponse request >>= return . Right) `catch` (return . Left)
  return $ either (Left . HTTPConnectionError)
                  (parseJson . BS.pack . LBS.unpack . responseBody)
                  result
  where
    getResponse request =
      withManager $ \manager -> httpLbs request manager

parseJson :: (FromJSON b, Show b) => BS.ByteString -> Either Error b
parseJson jsonString =
  handle $ parse (fromJSON <$> json) jsonString
  where
    handle (Data.Attoparsec.Done _ (Success s)) = Right s
    handle (Data.Attoparsec.Done _ (Error e)) =
      Left $ JsonError $ e ++ " on the JSON: " ++ BS.unpack jsonString
    handle (Fail _ _ e) = Left $ ParseError e
    handle (Partial k) = handle $ k ""
