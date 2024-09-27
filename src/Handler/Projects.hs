{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Projects
  ( getPrjsR, postPrjsR
  , getPrjR, postPrjR
  , getPrjNewR, getPrjEditR, postPrjDeleR
  , getPrjTeamR
  ) where

import ClassyPrelude (readMay)
import Control.Monad (void)

import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (?.), (==.), (:&)((:&))
    , Value (unValue), on, innerJoin, leftJoin, in_, subSelectMaybe, just
    , subSelectList
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR
      ( PrjsR, PrjR, PrjNewR, PrjEditR, PrjDeleR, TasksR, UserPhotoR
      , PrjTeamR
      )
    , AppMessage
      ( MsgSave, MsgCancel, MsgAlreadyExists
      , MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgRecordDeleted, MsgRecordEdited
      , MsgProjects, MsgProject, MsgNoProjectsYet, MsgPleaseAddIfNecessary
      , MsgLocation, MsgOutletType, MsgProjectStart, MsgProjectEnd, MsgTasks
      , MsgCode, MsgDele, MsgProjectManager, MsgNotAppointedYet, MsgManager
      , MsgPhoto, MsgManagerNotAssigned, MsgTeam, MsgDescription, MsgNoDescriptionGiven
      )
    )

import Material3 (md3widget, md3selectWidget, daytimeLocalField, md3textareaWidget)

import Model
    ( msgSuccess, msgError
    , Tasks (Tasks), Outlet (Outlet)
    , Empl(Empl), User (User), Task
    , PrjId
    , Prj
      ( Prj, prjCode, prjName, prjLocation, prjOutlet, prjStart, prjEnd
      , prjManager, prjDescr
      )
    , EntityField
      ( PrjCode, PrjId, OutletName, OutletId, PrjOutlet, PrjManager, EmplId
      , EmplUser, UserId, UserName, UserEmail, TaskOwner, TaskPrj
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages, addMessageI, redirect)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( textField, selectField, optionsPairs, Option (Option), OptionList (OptionList), textareaField)
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getPrjTeamR :: PrjId -> Handler Html
getPrjTeamR pid = do
    
    manager <- runDB $ selectOne $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        where_ $ just (x ^. EmplId) ==. subSelectMaybe ( do
            p <- from $ table @Prj
            where_ $ p ^. PrjId ==. val pid
            return $ p ^. PrjManager )
        return (x,u)

    owners <- runDB $ select $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        where_ $ just (x ^. EmplId) `in_` subSelectList ( do
            p <- from $ table @Task
            where_ $ p ^. TaskPrj ==. val pid
            return $ p ^. TaskOwner )
        return (x,u)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/prjs/team")


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
        setTitleI MsgProject
        idOverlay <- newIdent
        $(widgetFile "data/prjs/edit")


getPrjNewR :: Handler Html
getPrjNewR = do

    (fw,et) <- generateFormPost $ formProject Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
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

    (startR,startV) <- mreq daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgProjectStart
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . prjStart . entityVal <$> prj)

    (endR,endV) <- mreq daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgProjectEnd
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime utc . prjEnd . entityVal <$> prj)
    
    emplOptions <- liftHandler $ (option <$>) <$> runDB ( select $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        orderBy [asc (u ^. UserName), asc (u ^. UserEmail), asc (u ^. UserId)]
        return ((u ^. UserName, u ^. UserEmail), x ^. EmplId) )

    (managerR,managerV) <- mopt (selectField (pure $ optionsList $ options emplOptions)) FieldSettings
        { fsLabel = SomeMessage MsgProjectManager
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (prjManager . entityVal <$> prj)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (prjDescr . entityVal <$> prj)

    let r = Prj <$> typeR <*> codeR <*> nameR <*> locationR
            <*> (localTimeToUTC utc <$> startR)
            <*> (localTimeToUTC utc <$> endR)
            <*> managerR <*> descrR

    let w = $(widgetFile "data/prjs/form") 
    return (r,w)
  where

      option = bimap ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) unValue
      options = ((\(lbl, pid) -> Option lbl pid (pack $ show $ fromSqlKey pid)) <$>)
      optionsList = flip OptionList ((toSqlKey <$>) . readMay)
      
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
              setTitleI MsgProject
              idOverlay <- newIdent
              $(widgetFile "data/prjs/edit")


getPrjR :: PrjId -> Handler Html
getPrjR pid = do
    
    prj <- runDB $ selectOne $ do
        x :& t :& m :& u <- from $ table @Prj
            `innerJoin` table @Outlet `on` (\(x :& t) -> x ^. PrjOutlet ==. t ^. OutletId)
            `leftJoin` table @Empl `on` (\(x :& _ :& m) -> x ^. PrjManager ==. m ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& _ :& m :& u) -> m ?. EmplUser ==. u ?. UserId)
        where_ $ x ^. PrjId ==. val pid
        return ((x,t),(m,u))

    (fw0,et0) <- generateFormPost formPrjDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProject
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
              setTitleI MsgProject
              idOverlay <- newIdent
              $(widgetFile "data/prjs/new")


getPrjsR :: Handler Html
getPrjsR = do
    
    prjs <- runDB $ select $ do
        x :& m :& u <- from $ table @Prj
            `leftJoin` table @Empl `on` (\(x :& m) -> x ^. PrjManager ==. m ?. EmplId)
            `leftJoin` table @User `on` (\(_ :& m :& u) -> m ?. EmplUser ==. u ?. UserId)
        orderBy [asc (x ^. PrjId)]
        return (x,(m,u))
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgProjects 
        idOverlay <- newIdent
        $(widgetFile "data/prjs/prjs")

