{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  ) where

import ClassyPrelude (readMay)

import Control.Monad (unless)

import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), selectOne, from, table, countRows, select
    , (^.), (==.), (:&)((:&))
    , innerJoin, on, where_, val, just, valList, in_, groupBy
    )
import Database.Persist (Entity (Entity), entityKey)

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar, taskStati
    , Route(AuthR, HomeR, DataR)
    , DataR (PrjsR)
    , AppMessage
      ( MsgAppName, MsgHome, MsgWelcomeTo, MsgNoDescriptionGiven, MsgDescription
      , MsgSignIn, MsgLoginForMoreDetailsPlease, MsgStatistics, MsgShowProjects
      , MsgMyTasks, MsgLoginToCheckYourTasks, MsgTaskStatus, MsgCompletionDate
      , MsgProject, MsgTaskStatusPartiallyCompleted, MsgTaskStatusCompleted
      , MsgTaskStatusInProgress, MsgTaskStatusUncompleted, MsgTotalTasks
      , MsgTaskStatusInProgress, MsgTaskStatusNotStarted, MsgTotalProjects
      , MsgNoTasksWereFoundForSearchTerms
      )
    )
    
import Model
    ( paramTaskStatus, Prj (Prj), Task (Task), Empl
    , TaskStatus
      ( TaskStatusNotStarted, TaskStatusInProgress, TaskStatusCompleted
      , TaskStatusUncompleted, TaskStatusPartiallyCompleted
      )
    , EntityField (TaskOwner, EmplId, EmplUser, TaskPrj, PrjId, TaskStatus)
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, lookupGetParams
    )
import Yesod.Core.Handler
    ( getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth

    stati <- mapMaybe readMay <$> lookupGetParams paramTaskStatus

    tasks <- runDB $ select $ do
        x :& p :& o <- from $ table @Task
            `innerJoin` table @Prj `on` (\(x :& p) -> x ^. TaskPrj ==. p ^. PrjId)
            `innerJoin` table @Empl `on` (\(x :& _ :& o) -> x ^. TaskOwner ==. just (o ^. EmplId))
        where_ $ just (o ^. EmplUser) ==. val (entityKey <$> user)
        unless (null stati) $ where_ $ x ^. TaskStatus `in_` valList stati
        return (x,p)
    
    nprjs <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Prj
        return (countRows :: SqlExpr (Value Int)) )

    taskStats <- (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Task
        groupBy (x ^. TaskStatus)
        return (x ^. TaskStatus, countRows :: SqlExpr (Value Int)) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome 
        idOverlay <- newIdent
        $(widgetFile "homepage")
