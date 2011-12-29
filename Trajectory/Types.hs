{-# LANGUAGE DeriveDataTypeable #-}
module Trajectory.Types where

import Control.Exception (IOException)
import Data.Data (Typeable, Data)
import Data.Time.Clock (UTCTime(..))

-- | Errors have been tagged according to their source, so you can more easily
-- dispatch and handle them.
data Error =
    HTTPConnectionError IOException -- ^ A HTTP error occurred. The actual caught error is included, if available.
  | ParseError String -- ^ An error in the parser itself.
  | JsonError String -- ^ The JSON is malformed or unexpected.
  | UserError String -- ^ Incorrect input.
  deriving (Show, Eq)

-- | A time type representing Rails' TimeWithZone as its default string.
newtype TimeWithZone = TimeWithZone UTCTime deriving (Show, Eq, Typeable, Data)

-- | A Trajectory story.
data Story = Story {
   storyArchived :: Bool
  ,storyAssigneeId :: Maybe Int
  ,storyBranch :: Maybe String
  ,storyCreatedAt :: TimeWithZone
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
  ,storyUpdatedAt :: TimeWithZone
  ,storyUserId :: Int
  ,storyCommentsCount :: Int
  ,storyAssigneeName :: Maybe String
  ,storyUserName :: String
  ,storyStateEvents :: [String]
  ,storyIdeaSubject :: Maybe String
} deriving (Show, Eq, Typeable, Data)

-- | An iteration in Trajectory. The iterationStartsOn is the most
-- user-identifying string, though it changes with time. The @storyIterationId@
-- is the same as the @iterationId@.
data Iteration = Iteration {
   iterationAcceptedPoints :: Int
  ,iterationIsComplete :: Bool
  ,iterationCreatedAt :: TimeWithZone
  ,iterationEstimatedPoints :: Int
  ,iterationEstimatedVelocity :: Int
  ,iterationId :: Int
  ,iterationStartsOn :: String
  ,iterationStoriesCount :: Int
  ,iterationTeamStrength :: Int
  ,iterationUpdatedAt :: TimeWithZone
  ,iterationPercentComplete :: Int
  ,iterationIsCurrent :: Bool
  ,iterationUnstartedStoriesCount :: Int
  ,iterationAcceptedStoriesCount :: Int
  ,iterationStartedStoriesCount :: Int
  ,iterationDeliveredStoriesCount :: Int
  ,iterationCommentsCount :: Int
} deriving (Show, Eq, Typeable, Data)
