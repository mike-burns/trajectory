{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Data.Data

import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Control.Applicative( (<*>) )
import Data.Monoid (mconcat)

import Trajectory.Private.Config (withKey)
import Trajectory.Private.API (getStories, Story(..), Iteration(..))

main = do
  args <- cmdArgs lsStoryArgDefinition
  withKey (profileName args) $ \key -> do
      stories <- getStories key (accountName args) (projectName args)
      putStrLn $ either (\error -> "Error: " ++ show error)
                        (handle args)
                        stories

handle :: LsStoryArg -> ([Story],[Iteration]) -> String
handle args (stories,iterations) =
  let filters = buildFilters args iterations
      renderer = buildRenderer args in
    renderer $ filters `pipe` stories
  where
    pipe :: [ a -> a ] -> (a -> a)
    pipe = foldr (.) id

buildRenderer args [] = ""
buildRenderer args stories
  | onlyNext args && null nonMilestones = ""
  | detailedOutput args && onlyNext args =
    head $ map detailedFormatter nonMilestones
  | detailedOutput args =
    intercalate "\n\n" $ map detailedFormatter stories
  | onlyNext args =
    head $ map simpleFormatter nonMilestones
  | otherwise =
    intercalate "\n" $ map simpleFormatter stories
    where
      nonMilestones = skipMilestones stories
      skipMilestones = filter $ ("Milestone" /=) . storyTaskType

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

buildFilters :: LsStoryArg -> [Iteration] -> [ [Story] -> [Story] ]
buildFilters args iterations = filters <*> [args]
  where filters = [
          ideaFilter
          ,beforeMilestoneFilter
          ,estimationFilter
          ,assignmentFilter
          ,stateFilter
          ,neededFilter
          ,iterationFilter iterations
          ,deletionsFilter
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
        (storyTaskType story /= "Milestone") &&
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

neededFilter args
  | showDesignNeeded args && showDevelopmentNeeded args =
    filter (\story -> storyDesignNeeded story && storyDevelopmentNeeded story)
  | showDesignNeeded args = filter storyDesignNeeded
  | showDevelopmentNeeded args = filter storyDevelopmentNeeded
  | otherwise = id

iterationFilter iterations args
  | showAllIterations args = id
  | showCurrentIteration args && (present $ showIteration args) =
    filter (storyInIterations $ [currentIteration] ++ desiredIterations)
  | present $ showIteration args = filter (storyInIterations desiredIterations)
  | otherwise = filter (storyInIterations [currentIteration])
  where
    currentIteration = head $ filter iterationIsCurrent iterations
    present = not . null
    desiredIterations = filter (\iteration ->
      (iterationStartsOn iteration) `elem` (showIteration args)
      ) iterations
    storyInIterations its story =
      (storyIterationId story) `elem` (map iterationId its)

deletionsFilter args = filter (not . storyDeleted)

data LsStoryArg = LsStoryArg {
   profileName :: String

  ,ideaSummaries :: [String]
  ,showNullIdeas :: Bool
  ,beforeMilestone :: Maybe String
  ,showEstimate :: [Int]
  ,showUnestimated :: Bool
  ,showAssigned :: [String]
  ,showUnassigned :: Bool
  ,showDesignNeeded :: Bool
  ,showDevelopmentNeeded :: Bool

  ,showUnstarted :: Bool
  ,showStarted :: Bool
  ,showFinished :: Bool
  ,showDone :: Bool
  ,showAccepted :: Bool
  ,showRejected :: Bool

  ,showAllIterations :: Bool
  ,showCurrentIteration :: Bool
  ,showIteration :: [String]

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
  ,showDesignNeeded = def
     &= explicit
     &= name "design"
     &= help "Stories that need design"
  ,showDevelopmentNeeded = def
     &= explicit
     &= name "development"
     &= help "Stories that need development"
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
  ,showAllIterations = def
     &= explicit
     &= name "all-iterations"
     &= groupname "Iteration filtering"
     &= help "Stories from all iterations"
  ,showCurrentIteration = def
     &= explicit
     &= name "current-iteration"
     &= help "Stories from the current iteration"
  ,showIteration = def
     &= explicit
     &= name "iteration"
     &= help "Stories from the named iteration"
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
