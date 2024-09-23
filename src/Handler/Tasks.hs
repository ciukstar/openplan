{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Tasks
  ( getTasksR, postTasksR
  , getTaskR, postTaskR
  , getTaskNewR, getTaskEditR, postTaskDeleR
  ) where

import Control.Applicative ((<|>)) 
import Control.Monad (void, forM)

import Data.Bifunctor (bimap)
import qualified Data.List.Safe as LS (last)
import Data.Maybe (isJust)
import Data.Text (Text, pack)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (?.), (==.), (:&)((:&))
    , isNothing_, Value (unValue), innerJoin, leftJoin, on, just
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Widget, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (TasksR, TaskR, TaskNewR, TaskEditR, TaskDeleR, PrjR, PrjsR)
    , AppMessage
      ( MsgTasks, MsgTask, MsgSave, MsgCancel, MsgAlreadyExists
      , MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgDele, MsgNoSubtasksYet
      , MsgRecordDeleted, MsgPleaseAddIfNecessary
      , MsgRecordEdited, MsgSubtasks, MsgNoTasksForThisProjectYet
      , MsgParentTask, MsgStart, MsgEnd, MsgDepartment, MsgProject
      , MsgTaskStatusNotStarted, MsgTaskStatusCompleted, MsgTaskStatusInProgress
      , MsgTaskStatusUncompleted, MsgTaskStatusPartiallyCompleted, MsgTaskStatus, MsgSequence
      )
    )

import Model
    ( msgSuccess, msgError, Tasks (Tasks)
    , PrjId, Dept (Dept)
    , TaskId, Task(Task, taskName, taskParent, taskStart, taskEnd, taskDept, taskStatus)
    , TaskStatus
      ( TaskStatusNotStarted, TaskStatusInProgress, TaskStatusCompleted
      , TaskStatusUncompleted, TaskStatusPartiallyCompleted
      )
    , EntityField
      ( TaskId, TaskParent, TaskName, TaskPrj, DeptName, DeptId, TaskDept
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages, addMessageI, redirect, lookupGetParams)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Form.Fields (textField, selectField, optionsPairs, datetimeLocalField)
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvRequired, fvInput, fvLabel)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)
import Data.Time.Format (formatTime, defaultTimeLocale)


postTaskDeleR :: PrjId -> TaskId -> Tasks -> Handler Html
postTaskDeleR prjId did ps = do
    ((fr,_),_) <- runFormPost formTaskDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ TasksR prjId ps
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ TaskR prjId did ps


getTaskEditR :: PrjId -> TaskId -> Tasks -> Handler Html
getTaskEditR prjId did ps@(Tasks dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    task <- runDB $ selectOne $ do
        x <- from $ table @Task
        where_ $ x ^. TaskId ==. val did
        return x

    (fw,et) <- generateFormPost $ formDepartment prjId Nothing task
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTask
        idOverlay <- newIdent
        $(widgetFile "data/tasks/edit")


getTaskNewR :: PrjId -> Tasks -> Handler Html
getTaskNewR prjId ps@(Tasks dids) = do

    (fw,et) <- generateFormPost $ formDepartment prjId (LS.last dids) Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTask
        idOverlay <- newIdent
        $(widgetFile "data/tasks/new")


formDepartment :: PrjId -> Maybe TaskId -> Maybe (Entity Task) -> Form Task
formDepartment prjId did task extra = do

    deptOptions <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Dept
        orderBy [asc (x ^. DeptName)]
        return (x ^. DeptName, x ^. DeptId) )

    (deptR,deptV) <- mreq (selectField (optionsPairs deptOptions)) FieldSettings
        { fsLabel = SomeMessage MsgDepartment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (taskDept . entityVal <$> task)
    
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (taskName . entityVal <$> task)

    (startR,startV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgStart
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . taskStart . entityVal <$> task)

    (endR,endV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgEnd
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . taskEnd . entityVal <$> task)

    (statusR,statusV) <- mreq (selectField (optionsPairs statusOptions)) FieldSettings
        { fsLabel = SomeMessage MsgTaskStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (taskStatus . entityVal <$> task)

    parentOptions <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Task
        orderBy [asc (x ^. TaskName)]
        return (x ^. TaskName, x ^. TaskId) )

    (parentR,parentV) <- mopt (selectField (optionsPairs parentOptions)) FieldSettings
        { fsLabel = SomeMessage MsgParentTask
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((taskParent . entityVal <$> task) <|> pure did)

    let r = Task prjId <$> deptR<*> nameR 
            <*> (localTimeToUTC utc <$> startR)
            <*> (localTimeToUTC utc <$> endR)
            <*> statusR <*> pure Nothing <*> parentR

    let w = $(widgetFile "data/tasks/form")
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Task 
              where_ $ x ^. TaskName ==. val name
              where_ $ x ^. TaskPrj ==. val prjId
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity tid' _) -> case task of
              Nothing -> Left MsgAlreadyExists
              Just (Entity tid'' _) | tid' == tid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists

      statusOptions = [ (MsgTaskStatusNotStarted,TaskStatusNotStarted)
                      , (MsgTaskStatusInProgress,TaskStatusInProgress)
                      , (MsgTaskStatusCompleted,TaskStatusCompleted)
                      , (MsgTaskStatusUncompleted,TaskStatusUncompleted)
                      , (MsgTaskStatusPartiallyCompleted,TaskStatusPartiallyCompleted)
                      ]


postTaskR :: PrjId -> TaskId -> Tasks -> Handler Html
postTaskR prjId did ps@(Tasks dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    task <- runDB $ selectOne $ do
        x <- from $ table @Task
        where_ $ x ^. TaskId ==. val did
        return x

    ((fr,fw),et) <- runFormPost $ formDepartment prjId Nothing task
    case fr of
      FormSuccess r -> do
          runDB $ replace did r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ TaskR prjId did ps
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTask
              idOverlay <- newIdent
              $(widgetFile "data/tasks/edit")


getTaskR :: PrjId -> TaskId -> Tasks -> Handler Html
getTaskR prjId tid ps@(Tasks tids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> tid : tids
    
    task <- runDB $ selectOne $ do
        x :& d :& p <- from $ table @Task
            `innerJoin` table @Dept `on` (\(x :& d) -> x ^. TaskDept ==. d ^. DeptId)
            `leftJoin` table @Task `on` (\(x :& _ :& p) -> x ^. TaskParent ==. p ?. TaskId)
        where_ $ x ^. TaskId ==. val tid
        where_ $ x ^. TaskPrj ==. val prjId
        return ((x,d), p)

    (fw0,et0) <- generateFormPost formTaskDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTask
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/tasks/task")


formTaskDelete :: Form ()
formTaskDelete extra = return (pure (), [whamlet|#{extra}|])


postTasksR :: PrjId -> Tasks -> Handler Html
postTasksR prjId ps = do

    ((fr,fw),et) <- runFormPost $ formDepartment prjId Nothing Nothing

    case fr of
      FormSuccess r -> do
          void $ runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ TasksR prjId ps

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTask
              idOverlay <- newIdent
              $(widgetFile "data/tasks/new")


getTasksR :: PrjId -> Tasks -> Handler Html
getTasksR prjId ps@(Tasks []) = do 

    open <- lookupGetParams "o"
    
    trees@(TaskTree roots) <- fetchTasks prjId Nothing
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTasks
        idOverlay <- newIdent
        $(widgetFile "data/tasks/tasks")
        
getTasksR prjId ps@(Tasks tids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> tids 

    tasks <- runDB $ select $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val prjId
        where_ $ x ^. TaskParent ==. val (LS.last tids)
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTasks 
        idOverlay <- newIdent
        $(widgetFile "data/tasks/subtasks")


newtype TaskTree = TaskTree [(Entity Task, TaskTree)]


buildSnippet :: PrjId -> [Text] -> Maybe TaskId -> Tasks -> TaskTree -> Widget
buildSnippet prjId open msid ps@(Tasks tids) (TaskTree trees) =
    [whamlet|
      <div>
        $forall (Entity tid (Task _ _ name start end status _ _),trees@(TaskTree subtasks)) <- trees
          $with (pid,level) <- (pack $ show $ fromSqlKey tid,length tids + 1)
            $if (length subtasks) > 0
              <hr>
              <details #details#{pid} open
                ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'collapse_all' : 'expand_all'">
                <summary.row.surface-container>
                  <div>
                    <i.expand.padding.circle.border.wave style="margin-left:#{level}rem">
                      expand_all
                    <div.badge.primary style="inset: 0 -1rem auto auto">
                      #{level}
                    
                  <a.row.max.padding.wave href=@{DataR $ TaskR prjId tid ps}>
                    <div.max>
                      <h6.small>
                        #{name}
                        
                      <div.bold>
                        $case status
                          $of TaskStatusNotStarted
                            _{MsgTaskStatusNotStarted}
                          $of TaskStatusInProgress
                            _{MsgTaskStatusInProgress}
                          $of TaskStatusCompleted
                            _{MsgTaskStatusCompleted}
                          $of TaskStatusUncompleted
                            _{MsgTaskStatusUncompleted}
                          $of TaskStatusPartiallyCompleted
                            _{MsgTaskStatusPartiallyCompleted}
                        
                      <label>
                        $with fmt <- pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
                          <div>
                              <time.daytime datetime=#{fmt start}>
                                #{fmt start}
                          <div>
                            <time.daytime datetime=#{fmt end}>
                              #{fmt end}

                    <i>arrow_forward_ios

                ^{buildSnippet prjId open msid (Tasks (tids <> [tid])) trees}

            $else
              <hr>
              <div.row.surface-container>

                <div>
                  <div.padding style="margin-left:#{level}rem">
                  <div.badge.primary style="inset: 0 -1rem auto auto">
                    #{level}
                      
                <a.row.max.padding.surface-container.wave href=@{DataR $ TaskR prjId tid ps}>
                  <div.max>
                    <h6.small>
                      #{name}

                    <div.bold>
                      $case status
                        $of TaskStatusNotStarted
                          _{MsgTaskStatusNotStarted}
                        $of TaskStatusInProgress
                          _{MsgTaskStatusInProgress}
                        $of TaskStatusCompleted
                          _{MsgTaskStatusCompleted}
                        $of TaskStatusUncompleted
                          _{MsgTaskStatusUncompleted}
                        $of TaskStatusPartiallyCompleted
                          _{MsgTaskStatusPartiallyCompleted}

                    <label>
                      $with fmt <- pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
                        <div>
                            <time.daytime datetime=#{fmt start}>
                              #{fmt start}
                        <div>
                          <time.daytime datetime=#{fmt end}>
                            #{fmt end}

                  <i>arrow_forward_ios
            |]


fetchTasks :: PrjId -> Maybe TaskId -> Handler TaskTree
fetchTasks prjId tid = do
    tasks <- runDB ( select $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val prjId
        where_ $ case tid of
          Nothing -> isNothing_ $ x ^. TaskParent
          Just parent -> x ^. TaskParent ==. just (val parent)
        orderBy [asc (x ^. TaskId)]
        return x )

    TaskTree <$> forM tasks ( \p@(Entity parent _) -> (p,) <$> fetchTasks prjId (Just parent) )
