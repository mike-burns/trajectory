{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Data.Data

import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Control.Applicative( (<*>) )
import Data.Monoid (mconcat)

main = do
  args <- cmdArgs lsStoryArgDefinition
  key <- getKey $ profileName args
  stories <- getStories key (accountName args) (projectName args)

  putStrLn $ handle stories args

-- TODO:
getKey profileName = return "bc510c80a9077bde54cee1034bc37d09"
getStories key accountName projectName = return [story1, story2, story4, story3]
  where
    story1 = Story
               False
               (Just 33)
               (Just "performance-starts-with-non-zero-f")
               "2011-12-19T14:13:40-05:00"
               False
               False
               False
               116494
               (Just 4404)
               287071
               0
               100
               "accepted"
               "Bug"
               "Perf. calc. for a single account should begin no earlier than the first nonzero balance"
               "2011-12-20T22:29:50-05:00"
               6348
               5
               (Just "Mike Burns")
               "Benjamin Rahn"
               ["unaccept"]
               (Just "Performance Calculations")
    story2 = Story
               False
               (Just 33)
               (Just "perf-calc-for-account-group-should-begin-no-earlier-than-the-max-of-the-dates-f")
               "2011-12-19T14:30:06-05:00"
               False
               False
               False
               116499
               (Just 4404)
               287071
               0
               200
               "accepted"
               "Bug"
               "Perf. calc. for an account group should begin no earlier than the MAX of the dates of the constituent accounts' first nonzero balances."
               "2011-12-20T22:29:50-05:00"
               6348
               3
               (Just "Mike Burns")
               "Benjamin Rahn"
               ["unaccept"]
               (Just "Performance Calculations")
    story3 = Story
               False
               Nothing
               Nothing
               "2011-12-18T22:46:35-05:00"
               False
               False
               False
               116344
               Nothing
               290339
               4
               7200
               "unstarted"
               "Feature"
               "Advisor can edit a manually entered asset"
               "2011-12-20T22:29:50-05:00"
               6348
               1
               Nothing
               "Benjamin Rahn"
               ["start"]
               Nothing
    story4 = Story
               False
               Nothing
               Nothing
               "2011-12-18T23:24:05-05:00"
               False
               False
               False
               116358
               (Just 4194)
               290339
               0
               7300
               "unstarted"
               "Milestone"
               "Manual assets: delete & edit holdings, accounts"
               "2011-12-20T22:29:50-05:00"
               6348
               0
               Nothing
               "Benjamin Rahn"
               ["start"]
               (Just "Manually-Entered Holdings")




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

handle :: [Story] -> LsStoryArg -> String
handle stories args =
  let filters = buildFilters args
      renderer = buildRenderer args in
    renderer $ filters `pipe` stories
  where
    pipe :: [ a -> a ] -> (a -> a)
    pipe = foldr (.) id

buildRenderer args [] = ""
buildRenderer args stories
  | detailedOutput args && onlyNext args =
    head $ map detailedFormatter stories
  | detailedOutput args =
    intercalate "\n\n" $ map detailedFormatter stories
  | onlyNext args =
    head $ map simpleFormatter stories
  | otherwise =
    intercalate "\n" $ map simpleFormatter stories

detailedFormatter story =
  title ++ "\n" ++ origins ++ "\n" ++ overall
  where
    title = milestonePrefix story ++ storyTitle story
    overall = intercalate "\t" $ [
       "id: " ++ (show $ storyId story)
      ,"points: " ++ (show $ storyPoints story)
      ,"comments: " ++ (show $ storyCommentsCount story)
      ]
    origins = intercalate "\t" $ [
       (fromMaybe "unassigned" $ storyAssigneeName story)
      ,(maybe "free-floating" ("idea: " ++) $ storyIdeaSubject story)
      ]

simpleFormatter story = milestonePrefix story ++ storyTitle story

milestonePrefix story
  | storyTaskType story == "Milestone" = "^^^ "
  | otherwise = ""

buildFilters :: LsStoryArg -> [ [Story] -> [Story] ]
buildFilters args = filters <*> [args]
  where filters = [
          ideaFilter
          ,beforeMilestoneFilter
          ,estimationFilter
          ,assignmentFilter
          ,stateFilter
          ]

ideaFilter args
  | null summaries && not limitedToNullIdeas = id
  | null summaries && limitedToNullIdeas =
    filter (isNothing . storyIdeaSubject)
  | otherwise = filter ideaElements
    where
      summaries = ideaSummaries args
      limitedToNullIdeas = showNullIdeas args
      ideaElements story =
        maybe limitedToNullIdeas
              (\summary -> summary `elem` summaries)
              (storyIdeaSubject story)

beforeMilestoneFilter args
  | isNothing $ beforeMilestone args = id
  | otherwise = takeWhile notTheMilestone
    where
      (Just milestoneTitle) = beforeMilestone args
      notTheMilestone story =
        (storyTaskType story /= "milestone") &&
          (storyTitle story /= milestoneTitle)

estimationFilter args
  | null (showEstimate args) && not (showUnestimated args) = id
  | null $ showEstimate args =
    filter (\story -> (storyPoints story) == 0)
  | not $ showUnestimated args =
    filter (\story -> (storyPoints story) `elem` (showEstimate args))
  | otherwise =
    filter (\story -> (storyPoints story) `elem` [0]++(showEstimate args))

assignmentFilter args
  | null assignments && not limitedToUnassigned = id
  | null assignments = filter (isNothing . storyAssigneeName)
  | otherwise = filter assignmentElements
    where
      limitedToUnassigned = showUnassigned args
      assignments = showAssigned args
      assignmentElements story =
        maybe limitedToUnassigned
              (\assignment -> assignment `elem` assignments)
              (storyAssigneeName story)

stateFilter args =
  filter (\story -> (storyState story) `elem` states)
  where
    allStates = 
      ["unstarted", "started", "finished", "done", "accepted", "rejected"]
    stateBits = [(showUnstarted args)
                ,(showStarted args)
                ,(showFinished args)
                ,(showDone args)
                ,(showAccepted args)
                ,(showRejected args)]
    states =
      if or stateBits
         then map fst $ filter snd $ zip allStates stateBits
         else allStates

data LsStoryArg = LsStoryArg {
   profileName :: String

  ,ideaSummaries :: [String]
  ,showNullIdeas :: Bool
  ,beforeMilestone :: Maybe String
  ,showEstimate :: [Int]
  ,showUnestimated :: Bool
  ,showAssigned :: [String]
  ,showUnassigned :: Bool

  ,showUnstarted :: Bool
  ,showStarted :: Bool
  ,showFinished :: Bool
  ,showDone :: Bool
  ,showAccepted :: Bool
  ,showRejected :: Bool

  ,detailedOutput :: Bool
  ,onlyNext :: Bool

  ,accountName :: String
  ,projectName :: String
} deriving (Show, Data, Typeable)

lsStoryArgDefinition = LsStoryArg {
   profileName = "default"
     &= explicit
     &= name "profile"
     &= groupname "Common flags"
     &= help "The profile name to use [default]"
  ,ideaSummaries = def
     &= explicit
     &= name "idea"
     &= groupname "Filtering"
     &= help "Stories matching this idea"
  ,showNullIdeas = def
     &= explicit
     &= name "unset-idea"
     &= help "Stories without an idea"
  ,beforeMilestone = def
     &= explicit
     &= name "before-milestone"
     &= help "Stories before the given milestone"
  ,showUnestimated = def
     &= explicit
     &= name "unestimated"
     &= help "Unestimated stories"
  ,showEstimate = def
     &= explicit
     &= name "estimate"
     &= help "Stories with the given estimate"
  ,showUnassigned = def
     &= explicit
     &= name "unassigned"
     &= help "Unassigned stories"
  ,showAssigned = def
     &= explicit
     &= name "assigned"
     &= help "Stories assigned to the named person"
  ,showUnstarted = def
     &= explicit
     &= name "unstarted"
     &= groupname "Filtering on state"
     &= help "Unstarted stories"
  ,showStarted = def
     &= explicit
     &= name "started"
     &= help "Started stories"
  ,showFinished = def
     &= explicit
     &= name "finished"
     &= help "Finished stories"
  ,showDone = def
     &= explicit
     &= name "done"
     &= help "Stories in the done state"
  ,showAccepted = def
     &= explicit
     &= name "accepted"
     &= help "Stories which have been accepted"
  ,showRejected = def
     &= explicit
     &= name "rejected"
     &= help "Stories which have been rejected"
  ,onlyNext = def
     &= explicit
     &= name "next"
     &= groupname "Formatting"
     &= help "Only show the next matching story"
  ,detailedOutput = def
     &= explicit
     &= name "long"
     &= name "l"
     &= groupname "Formatting"
     &= help "Show story details"
  ,accountName = def &= argPos 0
  ,projectName = def &= argPos 1
} &= program "lsstory" &= helpArg [groupname "Common flags"]
