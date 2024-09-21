{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  , getPageR
  , getSiteHomeR
  ) where


import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T (replicate)
    
import Database.Esqueleto.Experimental
    ( SqlExpr, select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (?.), (==.), (:&)((:&))
    , leftJoin, on, just, innerJoin, subSelectCount, Value (Value, unValue)
    )
import Database.Persist
    ( Entity (Entity), SelectOpt (Asc), selectFirst, entityKey
    )
import qualified Database.Persist as P ((==.))

import Foundation
    ( Handler
    , widgetSnackbar, widgetTopbar
    , Route (DataR, PageR)
    , DataR (ItemPhotoR, HeaderLogoR)
    , AppMessage
      ( MsgAppName, MsgHome, MsgPhoto, MsgNumberOfPages, MsgEmptyPage
      , MsgLogo
      )
    )
    
import Model
    ( SiteId, Site (Site)
    , WebpageId, Webpage (Webpage)
    , ContentsType (ContentsTypeText, ContentsTypeHtml)
    , HeadingLevel
      ( HeadingLevelH1, HeadingLevelH2, HeadingLevelH3, HeadingLevelH4
      , HeadingLevelH5, HeadingLevelH6
      )
    , DocHeader (DocHeader)
    , EntityField
      ( SiteName, WebpageTitle, WebpageSite, WebpageId
      , DocHeaderPage, SiteId, DocBodyPage, ProductItem, ItemId, ProductDisplay, DocFooterPage, LogoHeader
      ), DocBody (DocBody), Product (Product), Item (Item), DocFooter (DocFooter), Logo (Logo)
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, setTitle, toHtml, redirect, notFound
    )
import Yesod.Core.Handler
    ( getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (runDB)
import Data.Bifunctor (Bifunctor(second, bimap))
import Control.Monad (forM)


getPageR :: SiteId -> WebpageId -> Handler Html
getPageR _sid pid = do

    page <- runDB $ selectOne $ do
        x :& h :& b <- from $ table @Webpage
            `leftJoin` table @DocHeader `on` (\(x :& h) -> just (x ^. WebpageId) ==. h ?. DocHeaderPage)
            `leftJoin` table @DocBody `on` (\(x :& _ :& b) -> just (x ^. WebpageId) ==. b ?. DocBodyPage)
        where_ $ x ^. WebpageId ==. val pid
        return (x,(h,b))

    logo <- runDB $ selectOne $ do
        x <- from $ table @Logo
        where_ $ just (x ^. LogoHeader) ==. val (entityKey <$> ((fst . snd) =<< page))
        return x

    products <- runDB $ select $ do
        x :& i <- from $ table @Product
            `innerJoin` table @Item `on` (\(x :& i) -> x ^. ProductItem ==. i ^. ItemId)
        where_ $ just (x ^. ProductDisplay) ==. val (entityKey <$> ((snd . snd) =<< page))
        return (x,i)
        
    defaultLayout $ do
        
        case page of
          Just (Entity _ (Webpage _ title _),_) -> setTitle (toHtml title)
          Nothing -> setTitleI MsgAppName
          
        $(widgetFile "site/blank")


getSiteHomeR :: SiteId -> Handler Html
getSiteHomeR sid = do

    site <- runDB $ selectOne $ do
        x <- from $ table @Site
        where_ $ x ^. SiteId ==. val sid
        return x

    case site of
      Just (Entity _ (Site _ _ hid)) -> case hid of
        Just pid -> redirect $ PageR sid pid
        Nothing -> do
            page <- runDB $ selectFirst [WebpageSite P.==. sid] [Asc WebpageId]
            case page of
              Just (Entity pid _) -> redirect $ PageR sid pid
              Nothing -> notFound
              
      Nothing -> do
          page <- runDB $ selectFirst [WebpageSite P.==. sid] [Asc WebpageId]
          case page of
            Just (Entity pid _) -> redirect $ PageR sid pid
            Nothing -> notFound
    


getHomeR :: Handler Html
getHomeR = do

    sites <- runDB $ select $ do
        x <- from $ table @Site
        orderBy [asc (x ^. SiteName)]
        return x

    pages <- forM sites $ \site@(Entity sid _) -> do
        y <- (second (bimap unValue (bimap unValue unValue)) <$>) <$> runDB ( select $ do
            x <- from $ table @Webpage
            
            let headers :: SqlExpr (Value Int)
                headers = subSelectCount $ do
                    h <- from $ table @DocHeader
                    where_ $ h ^. DocHeaderPage ==. x ^. WebpageId
            
            let bodies :: SqlExpr (Value Int)
                bodies = subSelectCount $ do
                    b <- from $ table @DocBody
                    where_ $ b ^. DocBodyPage ==. x ^. WebpageId
            
            let footers :: SqlExpr (Value Int)
                footers = subSelectCount $ do
                    b <- from $ table @DocFooter
                    where_ $ b ^. DocFooterPage ==. x ^. WebpageId
                    
            where_ $ x ^. WebpageSite ==. val sid
            orderBy [asc (x ^. WebpageTitle)]
            return (x,(headers,(bodies,footers))) )
        return (site,y)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome 
        idOverlay <- newIdent
        $(widgetFile "homepage")
