{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Monitor
  ( getMonitorR, getMonitorPrjR
  , getMonitorPrjChartR, getMonitorPrjTaskLogsR
  , getMonitorPrjGanttR
  ) where


import Control.Monad (join)

import Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Aeson as A (Value(Null,String))
import Data.Bifunctor (bimap, second)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time.Clock
    ( UTCTime, NominalDiffTime, nominalDiffTimeToSeconds, utctDay)
import Data.Time.Calendar (diffDays)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( SqlExpr, select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (?.), (==.), (:&)((:&)), (<=.), (>.)
    , Value (unValue), on, innerJoin, leftJoin, subSelectMaybe, just
    , subSelect, exists, sum_, groupBy, countRows, max_
    , not_
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar, msgTaskStatus
    , Route (DataR, StaticR)
    , DataR
      ( UserPhotoR, MonitorR, MonitorPrjR, MonitorPrjTaskLogsR
      , MonitorPrjGanttR, MonitorPrjChartR
      )
    , AppMessage
      ( MsgEffortHours, MsgProjects, MsgProject, MsgNoProjectsYet
      , MsgLocation, MsgOutletType, MsgProjectManager, MsgNotAppointedYet, MsgManager
      , MsgPhoto, MsgManagerNotAssigned, MsgDescription, MsgNoDescriptionGiven
      , MsgCompletionPercentage, MsgProjectStatus
      , MsgPlannedEffort, MsgActualEffort, MsgNumberOfTasks, MsgRemainingEffort
      , MsgProjectDuration, MsgCompletedTasks, MsgOngoingTasks
      , MsgOverdueTasks, MsgTaskLogs, MsgNoTaskLogsYet, MsgTimestamp, MsgAction
      , MsgRemarks, MsgChart, MsgGantt, MsgGoogle, MsgTaskId, MsgTaskName
      , MsgStartDate, MsgEndDate, MsgDuration, MsgPercentComplete, MsgDependencies
      , MsgJSGantt
      )
    )

import GHC.Float (int2Double)

import Model
    ( nominalDiffTimeToHours
    , Outlet (Outlet)
    , EmplId, Empl(Empl), User (User), Task (Task)
    , PrjId
    , Prj (Prj)
    , TaskLog (TaskLog)
    , TaskStatus
      ( TaskStatusCompleted
      )
    , EntityField
      ( PrjId, OutletId, PrjOutlet, PrjManager, EmplId, EmplUser, UserId
      , TaskPrj, TaskLogTask, TaskId, TaskLogEffort
      , TaskStatus, TaskLogTime, TaskEnd, TaskLogEmpl
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (js_echarts_min_js, js_jsgantt_js, css_jsgantt_css)

import Text.Printf (printf)
import Text.Hamlet (Html)
import Text.Julius (rawJS)

import Yesod.Core
    ( Yesod(defaultLayout), addScript, addScriptRemote, addStylesheet, array
    )
import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Aeson.Text (encodeToLazyText)


getMonitorPrjTaskLogsR :: EmplId -> PrjId -> Handler Html
getMonitorPrjTaskLogsR eid pid = do

    logs <- runDB $ select $ do
        x :& t :& e <- from $ table @TaskLog
            `innerJoin` table @Task `on` (\(x :& t) -> x ^. TaskLogTask ==. t ^. TaskId)
            `innerJoin` table @Empl `on` (\(x :& _ :& e) -> x ^. TaskLogEmpl ==. e ^. EmplId)
        where_ $ t ^. TaskPrj ==. val pid
        return ((x,t),e)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTaskLogs
        idOverlay <- newIdent
        $(widgetFile "data/monitor/logs")


getMonitorPrjGanttR :: EmplId -> PrjId -> Handler Html
getMonitorPrjGanttR eid pid = do

    tasks <- (GoogleGanttTask . second (fromMaybe 0 . join . unValue) <$>) <$> runDB ( select $ do
        x <- from $ table @Task
        let hours :: SqlExpr (Value (Maybe (Maybe NominalDiffTime)))
            hours = subSelect $ do
                l <- from $ table @TaskLog
                where_ $ l ^. TaskLogTask ==. x ^. TaskId
                return $ sum_ $ l ^. TaskLogEffort
        where_ $ x ^. TaskPrj ==. val pid
        orderBy [asc (x ^. TaskId)]
        return (x,hours) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTaskLogs
        idOverlay <- newIdent
        idChart <- newIdent
        
        addScriptRemote "https://www.gstatic.com/charts/loader.js"
         
        $(widgetFile "data/monitor/gantt")
    

getMonitorPrjChartR :: EmplId -> PrjId -> Handler Html
getMonitorPrjChartR eid pid = do

    tasks <- (GanttTask . second (fromMaybe 0 . join . unValue) <$>) <$> runDB ( select $ do
        x <- from $ table @Task
        let hours :: SqlExpr (Value (Maybe (Maybe NominalDiffTime)))
            hours = subSelect $ do
                l <- from $ table @TaskLog
                where_ $ l ^. TaskLogTask ==. x ^. TaskId
                return $ sum_ $ l ^. TaskLogEffort
        where_ $ x ^. TaskPrj ==. val pid
        orderBy [asc (x ^. TaskId)]
        return (x,hours) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTaskLogs
        idOverlay <- newIdent
        idChart <- newIdent
        
        addScript $ StaticR js_jsgantt_js
        addStylesheet $ StaticR css_jsgantt_css
         
        $(widgetFile "data/monitor/chart")


showUTCTime :: UTCTime -> Text 
showUTCTime = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"


newtype GoogleGanttTask = GoogleGanttTask (Entity Task,NominalDiffTime)

instance ToJSON GoogleGanttTask where
    toJSON :: GoogleGanttTask -> A.Value
    toJSON (GoogleGanttTask (Entity tid (Task _ _ name start end effort _ _status Nothing _owner _desr),hours)) =
        array [ A.String . pack . show . fromSqlKey $ tid
              , A.String name
              , A.String . showUTCTime $ start
              , A.String . showUTCTime $ end
              , A.Null
              , let p = if effort == 0
                        then 0
                        else
                          (nominalDiffTimeToHours hours / nominalDiffTimeToHours effort) * 100.0
                in A.String . pack $ printf "%.2f" p
              , A.Null
              ]
    
    toJSON (GoogleGanttTask (Entity tid (Task _ _ name start end effort _ _status (Just parent) _owner _desr),hours)) = 
        array [ A.String . pack . show . fromSqlKey $ tid
              , A.String name
              , A.String . showUTCTime $ start
              , A.String . showUTCTime $ end
              , A.Null
              , let p = if effort == 0
                        then 0
                        else
                          (nominalDiffTimeToHours hours / nominalDiffTimeToHours effort) * 100.0
                in A.String . pack $ printf "%.2f" p
              , A.String . pack . show . fromSqlKey $ parent
              ]
        

newtype GanttTask = GanttTask (Entity Task,NominalDiffTime)

instance ToJSON GanttTask where
    toJSON :: GanttTask -> A.Value
    toJSON (GanttTask (Entity tid (Task _ _ name start end effort _ _status Nothing _owner _desr),hours)) =
        object [ "pID" .= fromSqlKey tid
               , "pName" .= name
               , "pStart" .= showUTCTime start
               , "pEnd" .= showUTCTime end
               , "pComp" .= ( let p = if effort == 0
                                      then 0
                                      else
                                        (nominalDiffTimeToHours hours / nominalDiffTimeToHours effort) * 100.0
                              in (printf "%.2f" p :: String)
                            )
               , "pParent" .= (0 :: Int)
               , "pClass" .= ("gtaskblue" :: Text)
               ]
    
    toJSON (GanttTask (Entity tid (Task _ _ name start end effort _ _status (Just parent) _owner _desr),hours)) = 
        object [ "pID" .= fromSqlKey tid
               , "pName" .= name
               , "pStart" .= showUTCTime start
               , "pEnd" .= showUTCTime end
               , "pComp" .= ( let p = if effort == 0
                                      then 0
                                      else
                                        (nominalDiffTimeToHours hours / nominalDiffTimeToHours effort) * 100.0
                              in (printf "%.2f" p :: String)
                            )
               , "pParent" .= fromSqlKey parent
               , "pClass" .= ("gtaskblue" :: Text)
               , "pDepend" .= fromSqlKey parent
               ]


getMonitorPrjR :: EmplId -> PrjId -> Handler Html
getMonitorPrjR eid pid = do
    
    prj <- (second (fromMaybe 0 . join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& o :& m :& u <- from $ table @Prj
            `innerJoin` table @Outlet `on` (\(x :& t) -> x ^. PrjOutlet ==. t ^. OutletId)
            `leftJoin` table @Empl `on` (\(x :& _ :& m) -> x ^. PrjManager ==. m ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& _ :& m :& u) -> m ?. EmplUser ==. u ?. UserId)

        let actualEffort :: SqlExpr (Value (Maybe (Maybe NominalDiffTime)))
            actualEffort = subSelect $ do
                l :& t <- from $ table @TaskLog
                    `innerJoin` table @Task `on` (\(l :& t) -> l ^. TaskLogTask ==. t ^. TaskId)
                where_ $ t ^. TaskPrj ==. x ^. PrjId
                return $ sum_ $ l ^. TaskLogEffort
                
        where_ $ x ^. PrjId ==. val pid
        return (((x,o),(m,u)),actualEffort) )

    completedTasks <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val pid
        where_ $ x ^. TaskStatus ==. val TaskStatusCompleted
        return (countRows :: SqlExpr (Value Int)) )
                
    ongoingTasks <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val pid
        where_ $ not_ $ x ^. TaskStatus ==. val TaskStatusCompleted
        where_ $ exists $ do
            l <- from $ table @TaskLog
            where_ $ l ^. TaskLogTask ==. x ^. TaskId
            where_ $ l ^. TaskLogTime <=. x ^. TaskEnd
            where_ $ just (l ^. TaskLogTime) ==. subSelectMaybe ( do
                m <- from $ table @TaskLog
                where_ $ m ^. TaskLogTask ==. l ^. TaskLogTask
                return $ max_ $ m ^. TaskLogTime )
        return (countRows :: SqlExpr (Value Int)) )
    
    overdueTasks <- maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val pid
        where_ $ not_ $ x ^. TaskStatus ==. val TaskStatusCompleted
        where_ $ exists $ do
            l <- from $ table @TaskLog
            where_ $ l ^. TaskLogTask ==. x ^. TaskId
            where_ $ l ^. TaskLogTime >. x ^. TaskEnd
            where_ $ just (l ^. TaskLogTime) ==. subSelectMaybe ( do
                m <- from $ table @TaskLog
                where_ $ m ^. TaskLogTask ==. l ^. TaskLogTask
                return $ max_ $ m ^. TaskLogTime )
        return (countRows :: SqlExpr (Value Int)) )

    tasksByStatus <- (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Task
        where_ $ x ^. TaskPrj ==. val pid
        groupBy (x ^. TaskStatus)
        orderBy [asc (x ^. TaskStatus)]
        return (x ^. TaskStatus, countRows :: SqlExpr (Value Int)) )

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
        idOverlay <- newIdent
        idCardCompletionDegree <- newIdent
        idChartCompletionDegree <- newIdent
        addScript $ StaticR js_echarts_min_js
        $(widgetFile "data/monitor/prj")


getMonitorR :: EmplId -> Handler Html
getMonitorR eid = do
    
    prjs <- (second (fromMaybe 0 . join . unValue) <$>) <$> runDB ( select $ do
        x :& m :& u <- from $ table @Prj
            `leftJoin` table @Empl `on` (\(x :& m) -> x ^. PrjManager ==. m ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& m :& u) -> m ?. EmplUser ==. u ?. UserId)

        let actualEffort :: SqlExpr (Value (Maybe (Maybe NominalDiffTime)))
            actualEffort = subSelect $ do
                l :& t <- from $ table @TaskLog
                    `innerJoin` table @Task `on` (\(l :& t) -> l ^. TaskLogTask ==. t ^. TaskId)
                where_ $ t ^. TaskPrj ==. x ^. PrjId
                return $ sum_ $ l ^. TaskLogEffort
            
        where_ $ x ^. PrjManager ==. just (val eid)
        orderBy [asc (x ^. PrjId)]
        return ((x,(m,u)),actualEffort) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProjects
        idOverlay <- newIdent
        $(widgetFile "data/monitor/prjs")
  where
      nominalDiffTimeToDoubleSeconds =
          int2Double . truncate . (*) ((^) @_ @Integer  10 12) . nominalDiffTimeToSeconds
