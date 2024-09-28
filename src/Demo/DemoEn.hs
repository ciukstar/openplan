{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Time.Clock (getCurrentTime, addUTCTime)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( User (User, userEmail, userPassword, userAdmin, userName)
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    , Dept (Dept, deptCode, deptName, deptParent)
    , Outlet (Outlet, outletName, outletDescr)
    , Prj (prjOutlet, Prj, prjCode, prjName, prjLocation, prjStart, prjEnd, prjManager, prjDescr)
    , Task
      ( Task, taskPrj, taskDept, taskName, taskStart, taskEnd, taskDuration, taskParent
      , taskStatus, taskOwner, taskDescr
      )
    , TaskStatus (TaskStatusInProgress, TaskStatusNotStarted)
    , Empl (Empl, emplUser, emplDept, emplPosition, emplAppointment)
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    now <- liftIO getCurrentTime

    let oneDayTime = 24 * 60 * 60

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "marylopez"
    uid1 <- insert $ User { userEmail = "marylopez@xmail.edu"
                          , userPassword = Just pass1
                          , userName = Just "Mary Lopez"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "jjohnson"
    uid2 <- insert $ User { userEmail = "jjohnson@xmail.edu"
                          , userPassword = Just pass2
                          , userName = Just "John Johnson"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "jmaulsby"
    uid3 <- insert $ User { userEmail = "jmaulsby@xmail.edu"
                          , userPassword = Just pass3
                          , userName = Just "Julian Maulsby"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "vschoen"
    uid4 <- insert $ User { userEmail = "vschoen@xmail.edu"
                          , userPassword = Just pass4
                          , userName = Just "Valentina Schoen"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    dept1 <- insert Dept { deptCode = "Repairs"
                         , deptName = "Repairs"
                         , deptParent = Nothing
                         }

    dept11 <- insert Dept { deptCode = "RO 1"
                          , deptName = "Office #1"
                          , deptParent = Just dept1
                          }

    dept12 <- insert Dept { deptCode = "RO 2"
                          , deptName = "Office #2"
                          , deptParent = Just dept1
                          }

    dept2 <- insert Dept { deptCode = "IT"
                         , deptName = "Information Technologies"
                         , deptParent = Nothing
                         }

    dept21 <- insert Dept { deptCode = "IT o1"
                          , deptName = "Office #1"
                          , deptParent = Just dept2
                          }

    dept22 <- insert Dept { deptCode = "IT o2"
                          , deptName = "Office #2"
                          , deptParent = Just dept2
                          }

    pt1 <- insert Outlet { outletName = "Point Type #1"
                         , outletDescr = Just "The Point Type #1"
                         }

    pt2 <- insert Outlet { outletName = "Point Type #2"
                         , outletDescr = Just "The Point Type #2"
                         }

    pt3 <- insert Outlet { outletName = "Point Type #3"
                         , outletDescr = Just "The Point Type #3"
                         }

    empl1 <- insert Empl { emplUser = uid1
                         , emplDept = dept1
                         , emplPosition = "Accountant"
                         , emplAppointment = Just (addUTCTime ((-300) * oneDayTime) now)
                         }

    empl2 <- insert Empl { emplUser = uid2
                         , emplDept = dept11
                         , emplPosition = "IT engineer"
                         , emplAppointment = Just (addUTCTime ((-200) * oneDayTime) now)
                         }

    empl3 <- insert Empl { emplUser = uid3
                         , emplDept = dept2
                         , emplPosition = "Engineer"
                         , emplAppointment = Just (addUTCTime ((-250) * oneDayTime) now)
                         }

    empl4 <- insert Empl { emplUser = uid4
                         , emplDept = dept21
                         , emplPosition = "Architect"
                         , emplAppointment = Just (addUTCTime ((-650) * oneDayTime) now)
                         }

    let prj1 = Prj { prjOutlet = pt1
                   , prjCode = "P001"
                   , prjName = "Project #001"
                   , prjLocation = "1485 NW Street St Wilson WY 83014"
                   , prjStart = addUTCTime ((-30) * oneDayTime) now
                   , prjEnd = addUTCTime (40 * oneDayTime) now
                   , prjManager = Just empl1
                   , prjDescr = Just "This is the Project #001 with the code P001"
                   }

    p1 <- insert prj1

    let task11 = Task { taskPrj = p1
                      , taskDept = dept1
                      , taskName = "Task #010000000"
                      , taskStart = prjStart prj1
                      , taskEnd = addUTCTime oneDayTime (prjStart prj1)
                      , taskStatus = TaskStatusNotStarted
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      , taskOwner = Just empl2
                      , taskDescr = Just "Do that, do this."
                      }

    t11 <- insert task11

    let task111 = Task { taskPrj = p1
                       , taskDept = dept1
                       , taskName = "Task #011000000"
                       , taskStart = taskEnd task11
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task11)
                       , taskStatus = TaskStatusNotStarted
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t11
                       , taskOwner = Just empl3
                       , taskDescr = Just "Do that, do this.."
                       }
    t111 <- insert task111

    let task1111 = Task { taskPrj = p1
                        , taskDept = dept1
                        , taskName = "Task #011100000"
                        , taskStart = taskEnd task111
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task111)
                        , taskStatus = TaskStatusNotStarted
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t111
                        , taskOwner = Just empl4
                        , taskDescr = Just "Do that, do this..."
                        }

    t1111 <- insert task1111

    let task11111 = Task { taskPrj = p1
                         , taskDept = dept1
                         , taskName = "Task #011100000"
                         , taskStart = taskEnd task1111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task1111)
                         , taskStatus = TaskStatusNotStarted
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t1111
                         , taskOwner = Nothing
                         , taskDescr = Just "Do that, do this...."
                         }
    t11111 <- insert task11111

    let prj2 = Prj { prjOutlet = pt2
                   , prjCode = "P002"
                   , prjName = "Project #002"
                   , prjLocation = "102 W E ST Elkton VA 22827"
                   , prjStart = addUTCTime ((-40) * oneDayTime) now
                   , prjEnd = addUTCTime (50 * oneDayTime) now
                   , prjManager = Just empl2
                   , prjDescr = Just "This is the Project #002 with the code P002"
                   }

    p2 <- insert prj2

    let task21 = Task { taskPrj = p2
                      , taskDept = dept1
                      , taskName = "Task #020000000"
                      , taskStart = prjStart prj2
                      , taskEnd = addUTCTime oneDayTime (prjStart prj2)
                      , taskStatus = TaskStatusInProgress
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      , taskOwner = Just empl3
                      , taskDescr = Just "Do that, do this"
                      }

    t21 <- insert task21

    let task211 = Task { taskPrj = p2
                       , taskDept = dept2
                       , taskName = "Task #021000000"
                       , taskStart = taskEnd task21
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task21)
                       , taskStatus = TaskStatusInProgress
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t21
                       , taskOwner = Just empl3
                       , taskDescr = Just "Do that, do this"
                       }
    t211 <- insert task211

    let task2111 = Task { taskPrj = p2
                        , taskDept = dept2
                        , taskName = "Task #021100000"
                        , taskStart = taskEnd task211
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task211)
                        , taskStatus = TaskStatusInProgress
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t211
                        , taskOwner = Just empl4
                        , taskDescr = Just "Do that, do this"
                        }
    t2111 <- insert task2111

    let task21111 = Task { taskPrj = p2
                         , taskDept = dept1
                         , taskName = "Task #021100000"
                         , taskStart = taskEnd task2111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task2111)
                         , taskStatus = TaskStatusInProgress
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t2111
                         , taskOwner = Just empl1
                         , taskDescr = Just "Do that, do this"
                         }
    t21111 <- insert task21111

    prj3 <- insert Prj { prjOutlet = pt3
                       , prjCode = "P003"
                       , prjName = "Project #003"
                       , prjLocation = "2351 County Road 0000 N Yale IL 62481"
                       , prjStart = addUTCTime ((-45) * oneDayTime) now
                       , prjEnd = addUTCTime (65 * oneDayTime) now
                       , prjManager = Nothing
                       , prjDescr = Just "This is the Project #002 with the code P003"
                       }

    return ()
