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

module Model where

import ClassyPrelude.Yesod
    ( Typeable, Int, Text, Textarea, mkMigrate
    , mkPersist, persistFileWith, share, sqlSettings
    )

import Control.Monad (mapM)

import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just))
import Data.Ord (Ord)
import Data.Text (pack, unpack)

import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.TH (derivePersistField)

import Prelude (Double)

import Text.Hamlet (Html)
import Text.Read (Read, readMaybe)
import Text.Shakespeare.I18N (Lang)
import Text.Show (Show, show)

import Yesod.Core.Dispatch (PathMultiPiece, toPathMultiPiece, fromPathMultiPiece)
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))



data DisplayLayout = DisplayLayoutTable | DisplayLayoutList | DisplayLayoutCards
    deriving (Show, Read, Eq, Ord)
derivePersistField "DisplayLayout"


data ContentsType = ContentsTypeText | ContentsTypeHtml
    deriving (Show, Read, Eq, Ord)
derivePersistField "ContentsType"


data HeadingLevel = HeadingLevelH1 | HeadingLevelH2 | HeadingLevelH3
                  | HeadingLevelH4 | HeadingLevelH5 | HeadingLevelH6
    deriving (Show, Read, Eq, Ord)
derivePersistField "HeadingLevel"


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


newtype Webpages = Webpages { unWebpage :: [WebpageId]}

instance PathMultiPiece Webpages where
    toPathMultiPiece :: Webpages -> [Text]
    toPathMultiPiece (Webpages xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Webpages
    fromPathMultiPiece xs = Webpages <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


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

paramLang :: Text
paramLang = "lang"

paramBacklink :: Text
paramBacklink = "backlink"

paramTheme :: Text
paramTheme = "theme"

eventChangeTheme :: Text
eventChangeTheme = "changetheme"
