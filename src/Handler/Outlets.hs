{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Outlets
  ( getOutletsR, postOutletsR
  , getOutletR, postOutletR
  , getOutletNewR, getOutletEditR, postOutletDeleR
  ) where

import Control.Monad (void)

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (OutletsR, OutletR, OutletNewR, OutletEditR, OutletDeleR)
    , AppMessage
      ( MsgSave, MsgCancel, MsgAlreadyExists, MsgName, MsgRecordAdded
      , MsgInvalidFormData, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgDele, MsgRecordDeleted, MsgRecordEdited
      , MsgNoOutletsYet, MsgPleaseAddIfNecessary
      , MsgOutletType, MsgDescription, MsgOutletTypes
      )
    )

import Material3 (md3widget, md3textareaWidget)

import Model
    ( msgSuccess, msgError
    , OutletId, Outlet(Outlet, outletName, outletDescr)
    , EntityField (OutletId, OutletName)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage))
import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages, addMessageI, redirect)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields (textField, textareaField)
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postOutletDeleR :: OutletId -> Handler Html
postOutletDeleR oid = do
    ((fr,_),_) <- runFormPost formOutletDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete oid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR OutletsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ OutletR oid


getOutletEditR :: OutletId -> Handler Html
getOutletEditR oid = do

    outlet <- runDB $ selectOne $ do
        x <- from $ table @Outlet
        where_ $ x ^. OutletId ==. val oid
        return x

    (fw,et) <- generateFormPost $ formProject outlet
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOutletType
        idOverlay <- newIdent
        $(widgetFile "data/outlets/edit")


getOutletNewR :: Handler Html
getOutletNewR = do

    (fw,et) <- generateFormPost $ formProject Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOutletType
        idOverlay <- newIdent
        $(widgetFile "data/outlets/new")


formProject :: Maybe (Entity Outlet) -> Form Outlet
formProject outlet extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (outletName . entityVal <$> outlet)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (outletDescr . entityVal <$> outlet)

    let r = Outlet <$> nameR <*> descrR

    let w = $(widgetFile "data/outlets/form") 
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Outlet
              where_ $ x ^. OutletName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity oid' _) -> case outlet of
              Nothing -> Left MsgAlreadyExists
              Just (Entity oid'' _) | oid' == oid'' -> Right name
                                    | otherwise -> Left MsgAlreadyExists


postOutletR :: OutletId -> Handler Html
postOutletR oid = do

    outlet <- runDB $ selectOne $ do
        x <- from $ table @Outlet
        where_ $ x ^. OutletId ==. val oid
        return x

    ((fr,fw),et) <- runFormPost $ formProject outlet
    case fr of
      FormSuccess r -> do
          runDB $ replace oid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ OutletR oid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgOutletType
              idOverlay <- newIdent
              $(widgetFile "data/outlets/edit")


getOutletR :: OutletId -> Handler Html
getOutletR oid = do
    
    outlet <- runDB $ selectOne $ do
        x <- from $ table @Outlet
        where_ $ x ^. OutletId ==. val oid
        return x

    (fw0,et0) <- generateFormPost formOutletDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOutletType
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/outlets/outlet")


formOutletDelete :: Form ()
formOutletDelete extra = return (pure (), [whamlet|#{extra}|])


postOutletsR :: Handler Html
postOutletsR = do

    ((fr,fw),et) <- runFormPost $ formProject Nothing

    case fr of
      FormSuccess r -> do
          void $ runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR OutletsR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/outlets/new")


getOutletsR :: Handler Html
getOutletsR = do
    
    outlets <- runDB $ select $ do
        x <- from $ table @Outlet
        orderBy [asc (x ^. OutletId)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOutletTypes 
        idOverlay <- newIdent
        $(widgetFile "data/outlets/outlets")

