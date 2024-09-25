{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Tasks
  ( getTasksR, postTasksR
  , getTaskR, postTaskR
  , getTaskNewR, getTaskEditR, postTaskDeleR
  ) where

import ClassyPrelude (readMay)

import Control.Applicative ((<|>)) 
import Control.Monad (void, forM)

import Data.Bifunctor (bimap)
import qualified Data.List.Safe as LS (last)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (?.), (==.), (:&)((:&))
    , isNothing_, Value (unValue), innerJoin, leftJoin, on, just
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Handler, Widget, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (TasksR, TaskR, TaskNewR, TaskEditR, TaskDeleR, PrjR, PrjsR, UserPhotoR)
    , AppMessage
      ( MsgTasks, MsgTask, MsgSave, MsgCancel, MsgAlreadyExists
      , MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgDele, MsgNoSubtasksYet
      , MsgRecordDeleted, MsgPleaseAddIfNecessary, MsgTeam, MsgTaskOwner
      , MsgRecordEdited, MsgSubtasks, MsgNoTasksForThisProjectYet, MsgSequence
      , MsgParentTask, MsgStart, MsgEnd, MsgDepartment, MsgProject
      , MsgTaskStatusNotStarted, MsgTaskStatusCompleted, MsgTaskStatusInProgress
      , MsgTaskStatusUncompleted, MsgTaskStatusPartiallyCompleted, MsgTaskStatus, MsgPreviousTask, MsgFirstTaskInSequence, MsgNotAppointedYet, MsgPhoto, MsgOwner, MsgManagerNotAssigned, MsgOwnerNotAssigned
      )
    )
    
import Material3 (md3widget, daytimeLocalField, md3selectWidget)

import Model
    ( msgSuccess, msgError, Tasks (Tasks)
    , PrjId, Dept (Dept)
    , TaskId
    , Task
      ( Task, taskName, taskParent, taskStart, taskEnd, taskDept, taskStatus
      , taskOwner
      )
    , TaskStatus
      ( TaskStatusNotStarted, TaskStatusInProgress, TaskStatusCompleted
      , TaskStatusUncompleted, TaskStatusPartiallyCompleted
      )
    , Empl (Empl), User (User)
    , EntityField
      ( TaskId, TaskParent, TaskName, TaskPrj, DeptName, DeptId, TaskDept
      , EmplUser, UserId, UserName, UserEmail, EmplId, TaskOwner
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages, addMessageI, lookupGetParams
    , redirect
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( selectField, optionsPairs, Option (Option), OptionList (OptionList)
    , textField
    )
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


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

    (fw,et) <- generateFormPost $ formTask prjId Nothing task
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTask
        idOverlay <- newIdent
        $(widgetFile "data/tasks/edit")


getTaskNewR :: PrjId -> Tasks -> Handler Html
getTaskNewR prjId ps@(Tasks dids) = do

    (fw,et) <- generateFormPost $ formTask prjId (LS.last dids) Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTask
        idOverlay <- newIdent
        $(widgetFile "data/tasks/new")


formTask :: PrjId -> Maybe TaskId -> Maybe (Entity Task) -> Form Task
formTask prjId did task extra = do

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

    (startR,startV) <- mreq daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgStart
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . taskStart . entityVal <$> task)

    (endR,endV) <- mreq daytimeLocalField FieldSettings
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
        { fsLabel = SomeMessage MsgPreviousTask
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((taskParent . entityVal <$> task) <|> pure did)
    
    emplOptions <- liftHandler $ (option <$>) <$> runDB ( select $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        orderBy [asc (u ^. UserName), asc (u ^. UserEmail), asc (u ^. UserId)]
        return ((u ^. UserName, u ^. UserEmail), x ^. EmplId) )

    (ownerR,ownerV) <- mopt (selectField (pure $ optionsList $ options emplOptions)) FieldSettings
        { fsLabel = SomeMessage MsgTaskOwner 
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (taskOwner . entityVal <$> task)

    let r = Task prjId <$> deptR<*> nameR 
            <*> (localTimeToUTC utc <$> startR)
            <*> (localTimeToUTC utc <$> endR)
            <*> statusR <*> pure Nothing <*> parentR <*> ownerR

    let w = $(widgetFile "data/tasks/form")
    return (r,w)
  where

      option = bimap ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) unValue
      options = ((\(lbl, pid) -> Option lbl pid (pack $ show $ fromSqlKey pid)) <$>)
      optionsList = flip OptionList ((toSqlKey <$>) . readMay)
      
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

    ((fr,fw),et) <- runFormPost $ formTask prjId Nothing task
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
        x :& d :& p :& o :& u <- from $ table @Task
            `innerJoin` table @Dept `on` (\(x :& d) -> x ^. TaskDept ==. d ^. DeptId)
            `leftJoin` table @Task `on` (\(x :& _ :& p) -> x ^. TaskParent ==. p ?. TaskId)
            `leftJoin` table @Empl `on` (\(x :& _ :& _ :& o) -> x ^. TaskOwner ==. o ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& _ :& _  :& o :& u) -> o ?. EmplUser ==. u ?. UserId)
        where_ $ x ^. TaskId ==. val tid
        where_ $ x ^. TaskPrj ==. val prjId
        return (((x,d), p),(o,u))

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

    ((fr,fw),et) <- runFormPost $ formTask prjId Nothing Nothing

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


newtype TaskTree = TaskTree [((Entity Task,(Maybe (Entity Empl),Maybe (Entity User))), TaskTree)]


buildSnippet :: PrjId -> [Text] -> Maybe TaskId -> Tasks -> TaskTree -> Widget
buildSnippet prjId open msid ps@(Tasks tids) (TaskTree trees) =
    [whamlet|
      <div>
        $forall ((Entity tid (Task _ _ name start end status _ _ _),(owner,user)),trees@(TaskTree subtasks)) <- trees
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
                          <time.day datetime=#{fmt start}>
                            #{fmt start}
                          &mdash;
                          <time.day datetime=#{fmt end}>
                            #{fmt end}

                      <div.row>
                        $maybe Entity _ (Empl uid _ _ _) <- owner
                          <img.circle.tiny src=@{DataR $ UserPhotoR uid} alt=_{MsgPhoto} loading=lazy>
                          <div.max>
                            <label>
                              _{MsgOwner}

                            <div>
                              $maybe Entity _ (User email _ name _) <- user
                                $maybe name <- name
                                  #{name}
                                $nothing
                                  #{email}

                        $nothing
                          <label.italic>
                            _{MsgOwnerNotAssigned}

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
                        <time.day datetime=#{fmt start}>
                          #{fmt start}
                        &mdash;
                        <time.day datetime=#{fmt end}>
                          #{fmt end}

                    <div.row>
                      $maybe Entity _ (Empl uid _ _ _) <- owner
                        <img.circle.tiny src=@{DataR $ UserPhotoR uid} alt=_{MsgPhoto} loading=lazy>
                        <div.max>
                          <label>
                            _{MsgOwner}

                          <div>
                            $maybe Entity _ (User email _ name _) <- user
                              $maybe name <- name
                                #{name}
                              $nothing
                                #{email}

                      $nothing
                        <label.italic>
                          _{MsgOwnerNotAssigned}

                  <i>arrow_forward_ios
            |]


fetchTasks :: PrjId -> Maybe TaskId -> Handler TaskTree
fetchTasks prjId tid = do
    tasks <- runDB ( select $ do
        x :& o :& u <- from $ table @Task
            `leftJoin` table @Empl `on` (\(x :& o) -> x ^. TaskOwner ==. o ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& o :& u) -> o ?. EmplUser ==. u ?. UserId)
        where_ $ x ^. TaskPrj ==. val prjId
        where_ $ case tid of
          Nothing -> isNothing_ $ x ^. TaskParent
          Just parent -> x ^. TaskParent ==. just (val parent)
        orderBy [asc (x ^. TaskId)]
        return (x,(o,u)) )

    TaskTree <$> forM tasks ( \p@(Entity parent _,_) -> (p,) <$> fetchTasks prjId (Just parent) )
