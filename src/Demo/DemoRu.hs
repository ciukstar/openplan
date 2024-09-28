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
    , Prj (Prj, prjOutlet, prjCode, prjName, prjLocation, prjStart, prjEnd, prjManager, prjDescr)
    , Task
      ( Task, taskPrj, taskDept, taskName, taskStart, taskEnd, taskDuration, taskStatus
      , taskParent, taskOwner, taskDescr
      )
    , TaskStatus (TaskStatusInProgress, TaskStatusNotStarted)
    , Empl (Empl, emplUser, emplDept, emplPosition, emplAppointment)
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

    pass4 <- liftIO $ saltPass "sergeevaav"
    uid4 <- insert $ User { userEmail = "sergeevaav@mail.ru"
                          , userPassword = Just pass4
                          , userName = Just "Сергеева Александра Владимировна"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
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

    empl1 <- insert Empl { emplUser = uid1
                         , emplDept = dept1
                         , emplPosition = "Бухгалтер"
                         , emplAppointment = Just (addUTCTime ((-300) * oneDayTime) now)
                         }

    empl2 <- insert Empl { emplUser = uid2
                         , emplDept = dept11
                         , emplPosition = "ИТ-инженер"
                         , emplAppointment = Just (addUTCTime ((-200) * oneDayTime) now)
                         }

    empl3 <- insert Empl { emplUser = uid3
                         , emplDept = dept2
                         , emplPosition = "Инженер"
                         , emplAppointment = Just (addUTCTime ((-600) * oneDayTime) now)
                         }

    empl4 <- insert Empl { emplUser = uid4
                         , emplDept = dept21
                         , emplPosition = "Архитектор"
                         , emplAppointment = Just (addUTCTime ((-550) * oneDayTime) now)
                         }

    let prj1 = Prj { prjOutlet = pt1
                   , prjCode = "П001"
                   , prjName = "Проект №001"
                   , prjLocation = "Россия, г. Тверь, Сосновая ул., д. 1 кв.165"
                   , prjStart = addUTCTime ((-30) * oneDayTime) now
                   , prjEnd = addUTCTime (44 * oneDayTime) now
                   , prjManager = Just empl1
                   , prjDescr = Just "Это проект №001 с кодом П001."
                   }
               
    p1 <- insert prj1

    let task11 = Task { taskPrj = p1
                      , taskDept = dept1
                      , taskName = "Задача №010000000"
                      , taskStart = prjStart prj1
                      , taskEnd = addUTCTime oneDayTime (prjStart prj1)
                      , taskStatus = TaskStatusNotStarted
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      , taskOwner = Just empl2
                      , taskDescr = Just "Сделай то, сделай это."
                      }
                 
    t11 <- insert task11

    let task111 = Task { taskPrj = p1
                       , taskDept = dept1
                       , taskName = "Задача №011000000"
                       , taskStart = taskEnd task11
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task11)
                       , taskStatus = TaskStatusNotStarted
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t11
                       , taskOwner = Just empl3
                       , taskDescr = Just "Сделай то, сделай это.."
                       }
    t111 <- insert task111

    let task1111 = Task { taskPrj = p1
                        , taskDept = dept1
                        , taskName = "Задача №011100000"
                        , taskStart = taskEnd task111
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task111)
                        , taskStatus = TaskStatusNotStarted
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t111
                        , taskOwner = Just empl4
                        , taskDescr = Just "Сделай то, сделай это..."
                        }
    t1111 <- insert task1111

    let task11111 = Task { taskPrj = p1
                         , taskDept = dept1
                         , taskName = "Задача №011100000"
                         , taskStart = taskEnd task1111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task1111)
                         , taskStatus = TaskStatusNotStarted
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t1111
                         , taskOwner = Nothing
                         , taskDescr = Just "Сделай то, сделай это...."
                         }
    t11111 <- insert task11111


    let prj2 = Prj { prjOutlet = pt2
                       , prjCode = "П002"
                       , prjName = "Проект №002"
                       , prjLocation = "Россия, г. Тула, Колхозная ул., д. 19 кв.68"
                       , prjStart = addUTCTime ((-50) * oneDayTime) now
                       , prjEnd = addUTCTime (30 * oneDayTime) now
                       , prjManager = Just empl2
                       , prjDescr = Just "Это проект №002 с кодом П002."
                       }

    p2 <- insert prj2

    let task21 = Task { taskPrj = p2
                      , taskDept = dept1
                      , taskName = "Задача №020000000"
                      , taskStart = prjStart prj2
                      , taskEnd = addUTCTime oneDayTime (prjStart prj2)
                      , taskStatus = TaskStatusInProgress
                      , taskDuration = Just oneDayTime
                      , taskParent = Nothing
                      , taskOwner = Just empl3
                      , taskDescr = Just "Сделай то, сделай это"
                      }
                 
    t21 <- insert task21

    let task211 = Task { taskPrj = p2
                       , taskDept = dept2
                       , taskName = "Задача №021000000"
                       , taskStart = taskEnd task21
                       , taskEnd = addUTCTime (2 * oneDayTime) (taskEnd task21)
                       , taskStatus = TaskStatusInProgress
                       , taskDuration = Just (2 * oneDayTime)
                       , taskParent = Just t21
                       , taskOwner = Just empl4
                       , taskDescr = Just "Сделай то, сделай это"
                       }
    t211 <- insert task211

    let task2111 = Task { taskPrj = p2
                        , taskDept = dept2
                        , taskName = "Задача №021100000"
                        , taskStart = taskEnd task211
                        , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task211)
                        , taskStatus = TaskStatusInProgress
                        , taskDuration = Just (3 * oneDayTime)
                        , taskParent = Just t211
                        , taskOwner = Just empl1
                        , taskDescr = Just "Сделай то, сделай это"
                        }
    t2111 <- insert task2111

    let task21111 = Task { taskPrj = p2
                         , taskDept = dept1
                         , taskName = "Задача №021100000"
                         , taskStart = taskEnd task2111
                         , taskEnd = addUTCTime (3 * oneDayTime) (taskEnd task2111)
                         , taskStatus = TaskStatusInProgress
                         , taskDuration = Just (3 * oneDayTime)
                         , taskParent = Just t2111
                         , taskOwner = Just empl1
                         , taskDescr = Just "Сделай то, сделай это"
                         }
    t21111 <- insert task21111

    prj3 <- insert Prj { prjOutlet = pt3
                       , prjCode = "П003"
                       , prjName = "Проект №003"
                       , prjLocation = "Россия, г. Каспийск, Юбилейная ул., д. 10 кв.216"
                       , prjStart = addUTCTime ((-40) * oneDayTime) now
                       , prjEnd = addUTCTime (60 * oneDayTime) now
                       , prjManager = Nothing
                       , prjDescr = Just "Это проект №003 с кодом П003."
                       }

    return ()
