{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeApplications           #-}

module Model where

import ClassyPrelude.Yesod
    ( Typeable, Text, mkMigrate, mkPersist, persistFileWith
    , share, sqlSettings, readMay
    )

import Control.Monad (mapM)

import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq)
import Data.Fixed (Fixed (MkFixed))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just))
import qualified Data.Proxy as DP (Proxy)
import Data.Ord (Ord)
import Data.Text (pack, unpack)
import Data.Time.Clock
    ( UTCTime, NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)

import Database.Persist
    ( PersistField, PersistValue, toPersistValue, fromPersistValue
    , PersistValue (PersistInt64)
    )
import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (fromSqlKey, toSqlKey, PersistFieldSql, SqlType, sqlType)
import Database.Persist.Types (SqlType (SqlInt64))
import Database.Persist.TH (derivePersistField)

import GHC.Float (Double, int2Double, truncateDouble)
import GHC.Integer (Integer)
import GHC.Num ((*))
import GHC.Real ((/), (^))

import Prelude (Enum, Bounded, fromIntegral, truncate, minBound, maxBound)

import Text.Hamlet (Html)
import Text.Read (Read, readMaybe)
import Text.Show (Show, show)

import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch
    ( PathPiece, toPathPiece, fromPathPiece
    , PathMultiPiece, toPathMultiPiece, fromPathMultiPiece
    )
import Yesod.Form (Textarea)


data TaskStatus = TaskStatusNotStarted
                | TaskStatusInProgress
                | TaskStatusPaused
                | TaskStatusCompleted
                | TaskStatusUncompleted
                | TaskStatusPartiallyCompleted
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "TaskStatus"


instance PathPiece TaskStatus where
    toPathPiece :: TaskStatus -> Text
    toPathPiece = pack . show

    fromPathPiece :: Text -> Maybe TaskStatus
    fromPathPiece = readMay
    

taskStati :: [TaskStatus]
taskStati = [minBound .. maxBound]


instance PersistField NominalDiffTime where
    toPersistValue :: NominalDiffTime -> PersistValue
    toPersistValue x = PersistInt64 (truncate (nominalDiffTimeToSeconds x))

    fromPersistValue :: PersistValue -> Either Text NominalDiffTime
    fromPersistValue (PersistInt64 x) = Right (secondsToNominalDiffTime (fromIntegral x))
    fromPersistValue _ = Left "Invalid NominalDiffTime"


instance PersistFieldSql NominalDiffTime where
    sqlType :: DP.Proxy NominalDiffTime -> SqlType
    sqlType _ = SqlInt64

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


newtype Depts = Depts { unDepts :: [DeptId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece Depts where
    toPathMultiPiece :: Depts -> [Text]
    toPathMultiPiece (Depts xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Depts
    fromPathMultiPiece xs = Depts <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


newtype Tasks = Tasks { unTasks :: [TaskId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece Tasks where
    toPathMultiPiece :: Tasks -> [Text]
    toPathMultiPiece (Tasks xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Tasks
    fromPathMultiPiece xs = Tasks <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }


mediae :: [(Text,Text)]
mediae = [("s","small"),("m","medium"),("l","large")] :: [(Text,Text)]

langs :: [(Text,Text)]
langs = [("ru","RU"),("en","EN")]

msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"

keyThemeMode :: Text
keyThemeMode = "booklib_theme_mode"

paramTaskStatus :: Text
paramTaskStatus = "status"

paramUserId :: Text
paramUserId = "uid"

paramLang :: Text
paramLang = "lang"

paramBacklink :: Text
paramBacklink = "backlink"

paramTheme :: Text
paramTheme = "theme"

eventChangeTheme :: Text
eventChangeTheme = "changetheme"


nominalDiffTimeToHours :: NominalDiffTime -> Double
nominalDiffTimeToHours =
    (/ 3600.0) . int2Double . truncate . nominalDiffTimeToSeconds


hoursToNominalDiffTime :: Double -> NominalDiffTime
hoursToNominalDiffTime =
    secondsToNominalDiffTime . MkFixed . (* (^) @Integer @Integer 10 12) . truncateDouble @Integer . (* 3600)
