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
    , UserPhoto (UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    , Dept (Dept, deptCode, deptName, deptParent)
    , Outlet (Outlet, outletName, outletDescr)
    , Prj (prjOutlet, Prj, prjCode, prjName, prjLocation, prjStart, prjEnd), Task (Task, taskPrj, taskDept, taskName, taskStart, taskEnd, taskDuration, taskParent)
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

    let prj1 = Prj { prjOutlet = pt1
                   , prjCode = "P001"
                   , prjName = "Project #01"
                   , prjLocation = "1485 NW Street St Wilson WY 83014"
                   , prjStart = addUTCTime ((-30) * oneDayTime) now
                   , prjEnd = addUTCTime (40 * oneDayTime) now
                   }
               
    p1 <- insert prj1

    let task11 = Task { taskPrj = p1
                      , taskDept = dept1
                      , taskName = "Task #010000000"
                      , taskStart = prjStart prj1
                      , taskEnd = addUTCTime oneDayTime (prjStart prj1)
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      }
                 
    t11 <- insert task11

    let task111 = Task { taskPrj = p1
                       , taskDept = dept1
                       , taskName = "Task #011000000"
                       , taskStart = taskEnd task11
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task11)
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t11
                       }
    t111 <- insert task111

    let task1111 = Task { taskPrj = p1
                        , taskDept = dept1
                        , taskName = "Task #011100000"
                        , taskStart = taskEnd task111
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task111)
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t111
                        }
    t1111 <- insert task1111

    let task11111 = Task { taskPrj = p1
                         , taskDept = dept1
                         , taskName = "Task #011100000"
                         , taskStart = taskEnd task1111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task1111)
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t1111
                         }
    t11111 <- insert task11111

    let prj2 = Prj { prjOutlet = pt2
                   , prjCode = "P002"
                   , prjName = "Project #002"
                   , prjLocation = "102 W E ST Elkton VA 22827"
                   , prjStart = addUTCTime ((-40) * oneDayTime) now
                   , prjEnd = addUTCTime (50 * oneDayTime) now
                   }

    p2 <- insert prj2

    let task21 = Task { taskPrj = p2
                      , taskDept = dept1
                      , taskName = "Task #020000000"
                      , taskStart = prjStart prj2
                      , taskEnd = addUTCTime oneDayTime (prjStart prj2)
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      }
                 
    t21 <- insert task21

    let task211 = Task { taskPrj = p2
                       , taskDept = dept2
                       , taskName = "Task #021000000"
                       , taskStart = taskEnd task21
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task21)
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t21
                       }
    t211 <- insert task211

    let task2111 = Task { taskPrj = p2
                        , taskDept = dept2
                        , taskName = "Task #021100000"
                        , taskStart = taskEnd task211
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task211)
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t211
                        }
    t2111 <- insert task2111

    let task21111 = Task { taskPrj = p2
                         , taskDept = dept1
                         , taskName = "Task #021100000"
                         , taskStart = taskEnd task2111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task2111)
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t2111
                         }
    t21111 <- insert task21111

    prj3 <- insert Prj { prjOutlet = pt3
                       , prjCode = "P003"
                       , prjName = "Project #003"
                       , prjLocation = "2351 County Road 0000 N Yale IL 62481"
                       , prjStart = addUTCTime ((-45) * oneDayTime) now
                       , prjEnd = addUTCTime (65 * oneDayTime) now
                       }

    return ()
