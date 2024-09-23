{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRu (fillDemoRu) where

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
    , Prj (Prj, prjOutlet, prjCode, prjName, prjLocation, prjStart, prjEnd), Task (Task, taskPrj, taskDept, taskName, taskStart, taskEnd, taskDuration, taskParent)
    )
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRu = do

    now <- liftIO getCurrentTime

    let oneDayTime = 24 * 60 * 60

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    uid1 <- insert $ User { userEmail = "bulanovalm@mail.ru"
                          , userPassword = Just pass1
                          , userName = Just "Буланова Любовь Михайловна"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "petrovia"
    uid2 <- insert $ User { userEmail = "petrovia@mail.ru"
                          , userPassword = Just pass2
                          , userName = Just "Петров Иван Александрович"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "smirnovav"
    uid3 <- insert $ User { userEmail = "smirnovav@mail.ru"
                          , userPassword = Just pass3
                          , userName = Just "Смирнов Андрей Васильевич"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

 
    dept1 <- insert Dept { deptCode = "Ремонт"
                         , deptName = "Ремонт"
                         , deptParent = Nothing
                         }

    dept11 <- insert Dept { deptCode = "РО №1"
                          , deptName = "Ремонт, Офис №1"
                          , deptParent = Just dept1
                          }

    dept12 <- insert Dept { deptCode = "РО №2"
                          , deptName = "Ремонт, Офис №2"
                          , deptParent = Just dept1
                          }

    dept2 <- insert Dept { deptCode = "IT"
                         , deptName = "Информационные технологии"
                         , deptParent = Nothing
                         }

    dept21 <- insert Dept { deptCode = "ITО №1"
                          , deptName = "IT, Офис №1"
                          , deptParent = Just dept2
                          }

    dept22 <- insert Dept { deptCode = "ITО №2"
                          , deptName = "IT, Офис №2"
                          , deptParent = Just dept2
                          }

    pt1 <- insert Outlet { outletName = "Тип точки №1"
                         , outletDescr = Just "Это точка типа №1"
                         }

    pt2 <- insert Outlet { outletName = "Тип точки №2"
                         , outletDescr = Just "Это точка типа №2"
                         }

    pt3 <- insert Outlet { outletName = "Тип точки №3"
                         , outletDescr = Just "Это точка типа №3"
                         }

    let prj1 = Prj { prjOutlet = pt1
                   , prjCode = "П001"
                   , prjName = "Проект №001"
                   , prjLocation = "Россия, г. Тверь, Сосновая ул., д. 1 кв.165"
                   , prjStart = addUTCTime ((-30) * oneDayTime) now
                   , prjEnd = addUTCTime (44 * oneDayTime) now
                   }
               
    p1 <- insert prj1

    let task11 = Task { taskPrj = p1
                      , taskDept = dept1
                      , taskName = "Задача №010000000"
                      , taskStart = prjStart prj1
                      , taskEnd = addUTCTime oneDayTime (prjStart prj1)
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      }
                 
    t11 <- insert task11

    let task111 = Task { taskPrj = p1
                       , taskDept = dept1
                       , taskName = "Задача №011000000"
                       , taskStart = taskEnd task11
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task11)
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t11
                       }
    t111 <- insert task111

    let task1111 = Task { taskPrj = p1
                        , taskDept = dept1
                        , taskName = "Задача №011100000"
                        , taskStart = taskEnd task111
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task111)
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t111
                        }
    t1111 <- insert task1111

    let task11111 = Task { taskPrj = p1
                         , taskDept = dept1
                         , taskName = "Задача №011100000"
                         , taskStart = taskEnd task1111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task1111)
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t1111
                         }
    t11111 <- insert task11111


    let prj2 = Prj { prjOutlet = pt2
                       , prjCode = "П002"
                       , prjName = "Проект №002"
                       , prjLocation = "Россия, г. Тула, Колхозная ул., д. 19 кв.68"
                       , prjStart = addUTCTime ((-50) * oneDayTime) now
                       , prjEnd = addUTCTime (30 * oneDayTime) now
                       }

    p2 <- insert prj2

    let task21 = Task { taskPrj = p2
                      , taskDept = dept1
                      , taskName = "Задача №020000000"
                      , taskStart = prjStart prj2
                      , taskEnd = addUTCTime oneDayTime (prjStart prj2)
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      }
                 
    t21 <- insert task21

    let task211 = Task { taskPrj = p2
                       , taskDept = dept2
                       , taskName = "Задача №021000000"
                       , taskStart = taskEnd task21
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task21)
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t21
                       }
    t211 <- insert task211

    let task2111 = Task { taskPrj = p2
                        , taskDept = dept2
                        , taskName = "Задача №021100000"
                        , taskStart = taskEnd task211
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task211)
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t211
                        }
    t2111 <- insert task2111

    let task21111 = Task { taskPrj = p2
                         , taskDept = dept1
                         , taskName = "Задача №021100000"
                         , taskStart = taskEnd task2111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task2111)
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t2111
                         }
    t21111 <- insert task21111

    prj3 <- insert Prj { prjOutlet = pt3
                       , prjCode = "П003"
                       , prjName = "Проект №003"
                       , prjLocation = "Россия, г. Каспийск, Юбилейная ул., д. 10 кв.216"
                       , prjStart = addUTCTime ((-40) * oneDayTime) now
                       , prjEnd = addUTCTime (60 * oneDayTime) now
                       }

    return ()
