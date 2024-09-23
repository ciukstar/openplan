{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Projects
  ( getPrjsR, postPrjsR
  , getPrjR, postPrjR
  , getPrjNewR, getPrjEditR, postPrjDeleR
  ) where

import Control.Monad (void)

import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (==.), (:&)((:&))
    , Value (unValue), on, innerJoin
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (PrjsR, PrjR, PrjNewR, PrjEditR, PrjDeleR, TasksR)
    , AppMessage
      ( MsgDepartments, MsgDepartment, MsgSave, MsgCancel, MsgAlreadyExists
      , MsgCode, MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgDele, MsgRecordDeleted, MsgRecordEdited
      , MsgProjects, MsgProject, MsgTasks, MsgNoProjectsYet, MsgPleaseAddIfNecessary
      , MsgLocation, MsgOutletType, MsgProjectStart, MsgProjectEnd, MsgStart, MsgEnd
      )
    )

import Model
    ( msgSuccess, msgError, Tasks (Tasks)
    , PrjId, Prj(Prj, prjCode, prjName, prjLocation, prjOutlet, prjStart, prjEnd)
    , EntityField (PrjCode, PrjId, OutletName, OutletId, PrjOutlet), Outlet (Outlet)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core.Handler (newIdent, getMessageRender, getMessages, addMessageI, redirect)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Form.Fields (textField, selectField, optionsPairs, datetimeLocalField)
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvRequired, fvInput, fvLabel)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)


postPrjDeleR :: PrjId -> Handler Html
postPrjDeleR pid = do
    ((fr,_),_) <- runFormPost formPrjDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PrjsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ PrjR pid


getPrjEditR :: PrjId -> Handler Html
getPrjEditR pid = do

    prj <- runDB $ selectOne $ do
        x <- from $ table @Prj
        where_ $ x ^. PrjId ==. val pid
        return x

    (fw,et) <- generateFormPost $ formProject prj
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        $(widgetFile "data/prjs/edit")


getPrjNewR :: Handler Html
getPrjNewR = do

    (fw,et) <- generateFormPost $ formProject Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        $(widgetFile "data/prjs/new")


formProject :: Maybe (Entity Prj) -> Form Prj
formProject prj extra = do

    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (prjCode . entityVal <$> prj)

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (prjName . entityVal <$> prj)

    (locationR,locationV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgLocation
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (prjLocation . entityVal <$> prj)

        
    typeOptions <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Outlet
        orderBy [asc (x ^. OutletName)]
        return (x ^. OutletName, x ^. OutletId) )

    (typeR,typeV) <- mreq (selectField (optionsPairs typeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgOutletType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (prjOutlet . entityVal <$> prj)

    (startR,startV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgProjectStart
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . prjStart . entityVal <$> prj)

    (endR,endV) <- mreq datetimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgProjectEnd
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . prjEnd . entityVal <$> prj)

    let r = Prj <$> typeR <*> codeR <*> nameR <*> locationR
            <*> (localTimeToUTC utc <$> startR)
            <*> (localTimeToUTC utc <$> endR)

    let w = $(widgetFile "data/prjs/form") 
    return (r,w)
  where
      uniqueCodeField :: Field Handler Text
      uniqueCodeField = checkM uniqueCode textField

      uniqueCode :: Text -> Handler (Either AppMessage Text)
      uniqueCode code = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Prj
              where_ $ x ^. PrjCode ==. val code
              return x
          return $ case x of
            Nothing -> Right code
            Just (Entity pid' _) -> case prj of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid'' _) | pid' == pid'' -> Right code
                                    | otherwise -> Left MsgAlreadyExists


postPrjR :: PrjId -> Handler Html
postPrjR pid = do

    prj <- runDB $ selectOne $ do
        x <- from $ table @Prj
        where_ $ x ^. PrjId ==. val pid
        return x

    ((fr,fw),et) <- runFormPost $ formProject prj
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PrjR pid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgDepartment
              idOverlay <- newIdent
              $(widgetFile "data/prjs/edit")


getPrjR :: PrjId -> Handler Html
getPrjR pid = do
    
    prj <- runDB $ selectOne $ do
        x :& t <- from $ table @Prj
            `innerJoin` table @Outlet `on` (\(x :& t) -> x ^. PrjOutlet ==. t ^. OutletId)
        where_ $ x ^. PrjId ==. val pid
        return (x,t)

    (fw0,et0) <- generateFormPost formPrjDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/prjs/prj")


formPrjDelete :: Form ()
formPrjDelete extra = return (pure (), [whamlet|#{extra}|])


postPrjsR :: Handler Html
postPrjsR = do

    ((fr,fw),et) <- runFormPost $ formProject Nothing

    case fr of
      FormSuccess r -> do
          void $ runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PrjsR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/prjs/new")


getPrjsR :: Handler Html
getPrjsR = do
    
    prjs <- runDB $ select $ do
        x <- from $ table @Prj
        orderBy [asc (x ^. PrjId)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartments 
        idOverlay <- newIdent
        $(widgetFile "data/prjs/prjs")

