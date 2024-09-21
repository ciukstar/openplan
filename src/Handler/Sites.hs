{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Sites
  ( getSitesR, postSitesR
  , getSiteR, postSiteR
  , getSiteNewR, getSiteEditR, postSiteDeleR
  , getSiteFaviconR
  ) where

import Control.Monad (void)

import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( SqlExpr, select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (==.), (=.)
    , Value (unValue), update, set, subSelectCount
    )
import Database.Persist
    ( Entity (Entity), entityVal, replace, delete, entityKey
    )
import qualified Database.Persist as P
    ( upsert, upsertBy, (=.))


import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, SiteHomeR, FaviconR)
    , DataR
      ( SitesR, SiteR, SiteNewR, SiteDeleR, WebpagesR, SiteFaviconR
      )
    , AppMessage
      ( MsgSites, MsgSite, MsgName, MsgDescription, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgCopy
      , MsgPages, MsgHomepage, MsgUrl, MsgCreate, MsgSettings, MsgFavicon
      , MsgAttribution, MsgEmpty
      )
    )

import Model
    ( msgSuccess, msgError
    , SiteId, Site(Site, siteName, siteDescr, siteHome)
    , Webpage (Webpage)
    , Favicon (Favicon), Unique (UniqueSite)
    , EntityField
      ( SiteName, SiteId, WebpageSite, WebpageId, FaviconSite, SiteHome
      , FaviconAttribution, SiteDescr, FaviconMime, FaviconPhoto
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core
    ( Yesod(defaultLayout), getMessageRender, liftHandler, fileSourceByteString
    , FileInfo (fileContentType), TypedContent (TypedContent)
    , ToContent (toContent)
    )
import Yesod.Core.Handler
    (getMessages, newIdent, addMessageI, redirect)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields
    ( textField, textareaField, selectField, optionsPairs, fileField, htmlField)
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors, fvRequired, fvId
    )
import Yesod.Persist.Core (runDB)
import Data.Bifunctor (Bifunctor(second))


postSiteDeleR :: SiteId -> Handler Html
postSiteDeleR sid = do
    ((fr,_),_) <- runFormPost formSiteDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR SitesR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ SiteR sid


formSiteDelete :: Form ()
formSiteDelete extra = return (pure (), [whamlet|#{extra}|])


postSiteR :: SiteId -> Handler Html
postSiteR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    ((fr,fw),et) <- runFormPost $ formSite site
    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          void $ runDB $ replace sid r
          bs <- fileSourceByteString fi
          void $ runDB $ P.upsert (Favicon sid (fileContentType fi) bs attrib)
              [ FaviconMime P.=. fileContentType fi
              , FaviconPhoto P.=. bs
              , FaviconAttribution P.=. attrib
              ]
                
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SiteR sid
          
      FormSuccess (r,(Nothing,attrib)) -> do
          void $ runDB $ replace sid r
          
          runDB $ update $ \x -> do
                set x [ FaviconAttribution =. val attrib ]
                where_ $ x ^. FaviconSite ==. val sid
                
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SiteR sid
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/sites/edit")


getSiteEditR :: SiteId -> Handler Html
getSiteEditR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    (fw,et) <- generateFormPost $ formSite site

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/sites/edit")


postSitesR :: Handler Html
postSitesR = do

    ((fr,fw),et) <- runFormPost $ formSite Nothing

    case fr of
      FormSuccess (r@(Site name descr home),(Just fi,attrib)) -> do
          s <- runDB $ P.upsertBy (UniqueSite name) r
              [ SiteName P.=. name, SiteDescr P.=. descr, SiteHome P.=. home]
          
          bs <- fileSourceByteString fi
          void $ runDB $ P.upsert (Favicon (entityKey s) (fileContentType fi) bs attrib)
              [ FaviconMime P.=. fileContentType fi
              , FaviconPhoto P.=. bs
              , FaviconAttribution P.=. attrib
              ]
                
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR SitesR
          
      FormSuccess (r@(Site name descr home),(Nothing,attrib)) -> do
          s <- runDB $ P.upsertBy (UniqueSite name) r
              [ SiteName P.=. name, SiteDescr P.=. descr, SiteHome P.=. home]
          
          runDB $ update $ \x -> do
                set x [ FaviconAttribution =. val attrib ]
                where_ $ x ^. FaviconSite ==. val (entityKey s)
                
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR SitesR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              idForm <- newIdent
              $(widgetFile "data/sites/new")



getSiteNewR :: Handler Html
getSiteNewR = do

    (fw,et) <- generateFormPost $ formSite Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        idForm <- newIdent
        $(widgetFile "data/sites/new")


formSite :: Maybe (Entity Site) -> Form (Site,(Maybe FileInfo, Maybe Html))
formSite site extra = do

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteName . entityVal <$> site)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteDescr . entityVal <$> site)

    pages <- liftHandler $ runDB $ select $ do
        x <- from $ table @Webpage
        case site of
          Just (Entity sid _) -> where_ $ x ^. WebpageSite ==. val sid
          Nothing -> where_ $ val False
        orderBy [asc (x ^. WebpageId)]
        return x
        
    let homeOptions = optionsPairs ((\(Entity pid (Webpage _ name _)) -> (name,pid)) <$> pages)

    (homeR,homeV) <- mopt (selectField homeOptions) FieldSettings
        { fsLabel = SomeMessage MsgHomepage
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (siteHome . entityVal <$> site)

    (faviconR,faviconV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgFavicon
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case site of
      Just (Entity sid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @Favicon
          where_ $ x ^. FaviconSite ==. val sid
          return $ x ^. FaviconAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = (,) <$> (Site <$> nameR <*> descrR <*> homeR)
            <*> ((,) <$> faviconR <*> attribR)
            
    idFaviconContainer <- newIdent
    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    let w = $(widgetFile "data/sites/form")
    return (r,w)



getSiteR :: SiteId -> Handler Html
getSiteR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    (fw0,et0) <- generateFormPost formSiteDelete
    (fw,et) <- generateFormPost $ formSite site

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        idSiteUrl <- newIdent
        idButtonCopySiteUrl <- newIdent
        idForm <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/sites/site")
    

getSitesR :: Handler Html
getSitesR = do

    sites <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Site

        let n :: SqlExpr (Value Int)
            n = subSelectCount $ do
                p <- from $ table @Webpage
                where_ $ p ^. WebpageSite ==. x ^. SiteId
        
        orderBy [asc (x ^. SiteName)]
        return (x,n) )

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/sites/sites")

        
getSiteFaviconR :: SiteId -> Handler TypedContent
getSiteFaviconR sid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Favicon
        where_ $ x ^. FaviconSite ==. val sid
        return x
    case photo of
      Just (Entity _ (Favicon _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect FaviconR
