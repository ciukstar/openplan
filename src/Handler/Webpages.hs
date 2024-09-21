{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Webpages
  ( getWebpagesR, postWebpagesR
  , getWebpageR, postWebpageR
  , getWebpageNewR, getWebpageEditR, postWebpageDeleR
  ) where

import Data.Maybe (isJust)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (?.), (==.), (:&)((:&))
    , leftJoin, on, just
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert_, replace, delete
    )

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, PageR)
    , DataR
      ( WebpagesR, WebpageR, WebpageNewR, WebpageDeleR
      , SiteR, SitesR, HeaderR, BodyR, HeaderLogoR
      )
    , AppMessage
      ( MsgPages, MsgPage, MsgConfirmPlease, MsgLogo, MsgEmptyPage
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgTitle
      , MsgHeader, MsgBody, MsgFooter, MsgSite, MsgNoPagesOnThisSiteYet
      , MsgCreate, MsgSettings, MsgBackgroundColor, MsgPleaseAddIfNecessary
      )
    )

import Model
    ( msgSuccess, msgError
    , SiteId
    , WebpageId
    , Webpage (Webpage, webpageTitle, webpageBgColor)
    , DocHeader, DocBody, DocFooter
    , EntityField
      ( WebpageTitle, WebpageId, WebpageSite, DocHeaderPage, DocBodyPage
      , DocFooterPage
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core (Yesod(defaultLayout), getMessageRender, setTitleI)
import Yesod.Core.Handler (getMessages, newIdent, addMessageI, redirect)
import Yesod.Persist.Core (runDB)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors, fvRequired
    )


postWebpageDeleR :: SiteId -> WebpageId -> Handler Html
postWebpageDeleR sid pid = do
    ((fr,_),_) <- runFormPost formWebpageDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ WebpagesR sid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ WebpageR sid pid


formWebpageDelete :: Form ()
formWebpageDelete extra = return (pure (), [whamlet|#{extra}|])


postWebpageR :: SiteId -> WebpageId -> Handler Html
postWebpageR sid pid = do

    webpage <- runDB $ selectOne $ do
        x <- from $ table @Webpage
        where_ $ x ^. WebpageId ==. val pid
        return x

    ((fr,fw),et) <- runFormPost $ formWebpage sid webpage
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ WebpageR sid pid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/webpages/edit")


getWebpageEditR :: SiteId -> WebpageId -> Handler Html
getWebpageEditR sid pid = do

    webpage <- runDB $ selectOne $ do
        x <- from $ table @Webpage
        where_ $ x ^. WebpageId ==. val pid
        return x

    (fw,et) <- generateFormPost $ formWebpage sid webpage

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/webpages/edit")


postWebpagesR :: SiteId -> Handler Html
postWebpagesR sid = do

    ((fr,fw),et) <- runFormPost $ formWebpage sid Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ WebpagesR sid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/webpages/new")



getWebpageNewR :: SiteId -> Handler Html
getWebpageNewR sid = do

    (fw,et) <- generateFormPost $ formWebpage sid Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/webpages/new")


formWebpage :: SiteId -> Maybe (Entity Webpage) -> Form Webpage
formWebpage sid webpage extra = do

    (titleR,titleV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTitle
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (webpageTitle . entityVal <$> webpage)

    (bgR,bgV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgBackgroundColor
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (webpageBgColor . entityVal <$> webpage)

    let r = Webpage sid <$> titleR <*> bgR
    let w = $(widgetFile "data/webpages/form")
    return (r,w)


getWebpageR :: SiteId -> WebpageId -> Handler Html
getWebpageR sid pid = do

    webpage <- runDB $ selectOne $ do
        x <- from $ table @Webpage
        where_ $ x ^. WebpageId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formWebpageDelete
    (fw,et) <- generateFormPost $ formWebpage sid webpage

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPage
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/webpages/webpage")
    

getWebpagesR :: SiteId -> Handler Html
getWebpagesR sid = do 

    webpages <- runDB $ select $ do
        x :& h :& b :& f <- from $ table @Webpage
            `leftJoin` table @DocHeader `on` (\(x :& h) -> just (x ^. WebpageId) ==. h ?. DocHeaderPage)
            `leftJoin` table @DocBody `on` (\(x :& _ :& b) -> just (x ^. WebpageId) ==. b ?. DocBodyPage)
            `leftJoin` table @DocFooter `on` (\(x :& _ :& _ :& f) -> just (x ^. WebpageId) ==. f ?. DocFooterPage)
        where_ $ x ^. WebpageSite ==. val sid
        orderBy [asc (x ^. WebpageTitle)]
        return (x,(h,(b,f)))

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/webpages/webpages")
