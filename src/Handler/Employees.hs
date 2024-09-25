{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Employees
  ( getEmplsR, postEmplsR
  , getEmplR, postEmplR
  , getEmplNewR, getEmplEditR, postEmplDeleR
  , getEmployeePhotoR
  , getEmplProjectsR
  ) where

import Control.Monad (void)

import Data.Bifunctor (bimap)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, utc, localTimeToUTC)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (==.), (:&)((:&))
    , Value (unValue), innerJoin, on
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR, StaticR)
    , DataR
      ( EmplsR, EmplR, EmplNewR, EmplEditR, EmplDeleR, DeptsR, DeptR
      , UserPhotoR, EmployeePhotoR
      )
    , AppMessage
      ( MsgEmployees, MsgEmployee, MsgSave, MsgCancel, MsgPhoto
      , MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgDele, MsgDetails
      , MsgRecordDeleted, MsgPleaseAddIfNecessary, MsgDivisions
      , MsgRecordEdited, MsgNoEmployeesInThisDepartmentYet
      , MsgDepartment, MsgJobTitle, MsgAppointmentDate, MsgUser
      , MsgProjects, MsgTasks
      )
    )

import Material3 (md3widget, md3selectWidget, daytimeLocalField)

import Model
    ( msgSuccess, msgError, paramUserId
    , EmplId, Empl(Empl, emplUser, emplPosition, emplAppointment)
    , DeptId, Depts (Depts), User (User)
    , EntityField
      ( EmplId, EmplDept, EmplUser, UserId, UserName, UserEmail
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core
    (Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Core.Handler
    ( newIdent, getMessageRender, getMessages, addMessageI, redirect)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( textField, selectField, intField, Option (Option), OptionList (OptionList))
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Input (ireq, runInputGet)
import ClassyPrelude (readMay)


getEmplProjectsR :: DeptId -> EmplId -> Depts -> Handler Html
getEmplProjectsR _did _eid _ps@(Depts _dids) = undefined


postEmplDeleR :: DeptId -> EmplId -> Depts -> Handler Html
postEmplDeleR did eid ps = do
    ((fr,_),_) <- runFormPost formEmplDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete eid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ EmplsR did ps
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ EmplR did eid ps


getEmplEditR :: DeptId -> EmplId -> Depts -> Handler Html
getEmplEditR did eid ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    empl <- runDB $ selectOne $ do
        x <- from $ table @Empl
        where_ $ x ^. EmplId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEmployee did empl
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        idOverlay <- newIdent
        $(widgetFile "data/depts/empls/edit")


getEmplNewR :: DeptId -> Depts -> Handler Html
getEmplNewR did ps = do

    (fw,et) <- generateFormPost $ formEmployee did Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        idOverlay <- newIdent
        $(widgetFile "data/depts/empls/new")


formEmployee :: DeptId -> Maybe (Entity Empl) -> Form Empl
formEmployee did empl extra = do

    let option = bimap ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) unValue
    
    userOptions <- liftHandler $ (option <$>) <$> runDB ( select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), asc (x ^. UserEmail), asc (x ^. UserId)]
        return ((x ^. UserName, x ^. UserEmail), x ^. UserId) )

    (userR,userV) <- mreq (selectField (pure $ optionsList $ options userOptions)) FieldSettings
        { fsLabel = SomeMessage MsgUser
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (emplUser . entityVal <$> empl)
    
    (positionR,positionV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgJobTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (emplPosition . entityVal <$> empl)

    (appointmentR,appointmentV) <- mopt daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgAppointmentDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } ((utcToLocalTime utc <$>) . emplAppointment . entityVal <$> empl)

    let r = Empl <$> userR <*> pure did <*> positionR
            <*> ((localTimeToUTC utc <$>) <$> appointmentR)

    idImgPhoto <- newIdent
    let w = $(widgetFile "data/depts/empls/form")
    return (r,w)
  where

      options = ((\(lbl, pid) -> Option lbl pid (pack $ show $ fromSqlKey pid)) <$>)
      optionsList = flip OptionList ((toSqlKey <$>) . readMay)


postEmplR :: DeptId -> EmplId -> Depts -> Handler Html
postEmplR did eid ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    empl <- runDB $ selectOne $ do
        x <- from $ table @Empl
        where_ $ x ^. EmplId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEmployee did empl
    case fr of
      FormSuccess r -> do
          runDB $ replace eid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ EmplR did eid ps
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgEmployee
              idOverlay <- newIdent
              $(widgetFile "data/depts/empls/edit")


getEmplR :: DeptId -> EmplId -> Depts -> Handler Html
getEmplR did eid ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids
    
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        where_ $ x ^. EmplId ==. val eid
        return (x,u)

    (fw0,et0) <- generateFormPost formEmplDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/depts/empls/empl")


formEmplDelete :: Form ()
formEmplDelete extra = return (pure (), [whamlet|#{extra}|])


postEmplsR :: DeptId -> Depts -> Handler Html
postEmplsR did ps = do

    ((fr,fw),et) <- runFormPost $ formEmployee did Nothing

    case fr of
      FormSuccess r -> do
          void $ runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ EmplsR did ps

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgEmployee
              idOverlay <- newIdent
              $(widgetFile "data/depts/empls/new")


getEmplsR :: DeptId -> Depts -> Handler Html
getEmplsR did ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> dids 

    empls <- runDB $ select $ do
        x :& u <- from $ table @Empl
            `innerJoin` table @User `on` (\(x :& u) -> x ^. EmplUser ==. u ^. UserId)
        where_ $ x ^. EmplDept ==. val did
        return (x,u)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployees 
        idOverlay <- newIdent
        $(widgetFile "data/depts/empls/empls")


getEmployeePhotoR :: Handler ()
getEmployeePhotoR = do
    uid <- toSqlKey <$> runInputGet ( ireq intField paramUserId )
    redirect $ DataR $ UserPhotoR uid
