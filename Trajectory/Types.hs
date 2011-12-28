{-# LANGUAGE DeriveDataTypeable #-}
module Trajectory.Types where

import Control.Exception (IOException)
import Data.Data (Typeable, Data)

-- | Errors have been tagged according to their source, so you can more easily
-- dispatch and handle them.
data Error =
    HTTPConnectionError IOException -- ^ A HTTP error occurred. The actual caught error is included, if available.
  | ParseError String -- ^ An error in the parser itself.
  | JsonError String -- ^ The JSON is malformed or unexpected.
  | UserError String -- ^ Incorrect input.
  deriving (Show, Eq)

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

data Iteration = Iteration {
   iterationAcceptedPoints :: Int
  ,iterationIsComplete :: Bool
  ,iterationCreatedAt :: String
  ,iterationEstimatedPoints :: Int
  ,iterationEstimatedVelocity :: Int
  ,iterationId :: Int
  ,iterationStartsOn :: String
  ,iterationStoriesCount :: Int
  ,iterationTeamStrength :: Int
  ,iterationUpdatedAt :: String
  ,iterationPercentComplete :: Int
  ,iterationIsCurrent :: Bool
  ,iterationUnstartedStoriesCount :: Int
  ,iterationAcceptedStoriesCount :: Int
  ,iterationStartedStoriesCount :: Int
  ,iterationDeliveredStoriesCount :: Int
  ,iterationCommentsCount :: Int
} deriving (Show, Eq, Typeable, Data)
