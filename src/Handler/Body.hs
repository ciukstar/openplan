{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Body
  ( getBodyR, postBodyR
  , getBodyItemsR, postBodyItemsR
  ) where

import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T (replicate)

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, where_, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , val, just, orderBy, asc, Value (unValue)
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, upsertBy, (=.))
import qualified Database.Persist as P
    ( insertMany, (==.), PersistQueryWrite (deleteWhere)
    )

import Foundation
    ( Handler, Form
    , widgetSnackbar, widgetTopbar
    , Route (DataR, PageR)
    , DataR
      ( WebpagesR, WebpageR, HeaderR, BodyR, BodyItemsR
      , ItemPhotoR
      )
    , AppMessage
      ( MsgBody, MsgPage, MsgCancel, MsgSave, MsgSettings, MsgHeader
      , MsgFooter, MsgBackgroundColor, MsgTable, MsgList, MsgCards
      , MsgLayout, MsgRecordEdited, MsgAddProduct, MsgPhoto, MsgItems
      , MsgYouCanAddSomeFromProductList, MsgNoProductsAssignedYet
      , MsgCreate
      )
    )
    
import Model
    ( msgSuccess
    , SiteId, WebpageId
    , DisplayLayout (DisplayLayoutTable, DisplayLayoutList, DisplayLayoutCards)
    , Unique (UniqueDocBody)
    , Product (Product)
    , ItemId, Item (Item)
    , DocBodyId, DocBody (DocBody, docBodyBgColor, docBodyLayout)
    , EntityField
      ( DocBodyBgColor, DocBodyLayout, ProductDisplay, ProductItem, ItemId
      , ItemName, DocBodyPage
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , SomeMessage (SomeMessage), MonadHandler (liftHandler)
    , addMessageI, redirect, whamlet, handlerToWidget
    )
import Yesod.Form
    ( colorField, selectField, optionsPairs
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId, fvInput, fvLabel, fvErrors, fvRequired)
    , FormResult (FormSuccess), mreq, Field (fieldView)
    , OptionList (olOptions), multiSelectField
    , Option (optionInternalValue, optionExternalValue)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mopt)
import Yesod.Persist.Core (YesodPersist(runDB))
import Control.Monad (void)


postBodyItemsR :: SiteId -> WebpageId -> DocBodyId -> Handler Html
postBodyItemsR sid pid bid = do

    selected <- (unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Product
        where_ $ x ^. ProductDisplay ==. val bid
        return $ x ^. ProductItem )

    ((fr,fw),et) <- runFormPost $ formItems selected
    case fr of
      FormSuccess r -> do
          runDB $ P.deleteWhere [ProductDisplay P.==. bid]
          void $ runDB $ P.insertMany ((`Product` bid) <$> r)
          redirect $ DataR $ BodyR sid pid
      _otherwise -> do
          msgr <- getMessageRender
          defaultLayout $ do
              setTitleI MsgBody
              idOverlay <- newIdent
              $(widgetFile "data/body/items/items")


getBodyItemsR :: SiteId -> WebpageId -> DocBodyId -> Handler Html
getBodyItemsR sid pid bid = do

    selected <- (unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Product
        where_ $ x ^. ProductDisplay ==. val bid
        return $ x ^. ProductItem )

    (fw,et) <- generateFormPost $ formItems selected
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgBody
        idOverlay <- newIdent
        $(widgetFile "data/body/items/items")


formItems :: [ItemId] -> Form [ItemId]
formItems selected extra = do

    items <- liftHandler $ runDB $ select $ do
        x <- from $ table @Item
        orderBy [asc (x ^. ItemName), asc (x ^. ItemId)]
        return x
    
    let itemOptions = optionsPairs ((\(Entity iid (Item name _ _ _ _ _)) -> (name,iid)) <$> items)
          
    (itemsR,itemsV) <- mreq (itemsField items itemOptions) "" (Just selected)

    return (itemsR, [whamlet|#{extra} ^{fvInput itemsV}|])
  where
      
      itemsField :: Eq a => [Entity Item] -> Handler (OptionList a) -> Field Handler [a]
      itemsField items ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs value _isReq -> do
                opts <- olOptions <$> handlerToWidget ioptlist
                let optselected (Left _) _ = False
                    optselected (Right vals) opt = optionInternalValue opt `elem` vals
                [whamlet|
<div ##{theId}>
  $forall (Entity iid (Item iname _ price currency rating _),opt) <- zip items opts
    <label.row.padding.surface-container.wave>

      <img.circle.extra src=@{DataR $ ItemPhotoR iid} alt=_{MsgPhoto}>
        
      <div.max>
        <h6.small>#{iname}
        <div.currency data-value=#{price} data-currency=#{currency}>
          #{price} #{currency}
        <div>
          $maybe r <- rating
            #{T.replicate r "☆"}
        
      <div.checkbox>
        <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected value opt:checked>
        <span>

    <hr>
                        |]
          }
    


postBodyR :: SiteId -> WebpageId -> Handler Html
postBodyR sid pid = do

    body <- runDB $ selectOne $ from $ table @DocBody
        
    ((fr,fw),et) <- runFormPost $ formBody pid body

    products <- runDB $ select $ do
        x :& i <- from $ table @Product
            `innerJoin` table @Item `on` (\(x :& i) -> x ^. ProductItem ==. i ^. ItemId)
        where_ $ just (x ^. ProductDisplay) ==. val (entityKey <$> body)
        return (x,i)

    case fr of
      FormSuccess r@(DocBody _ bgColor layout) -> do
          void $ runDB $ upsertBy (UniqueDocBody pid) r
              [ DocBodyBgColor =. bgColor
              , DocBodyLayout =. layout
              ]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ BodyR sid pid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBody
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/body/body")


getBodyR :: SiteId -> WebpageId -> Handler Html
getBodyR sid pid = do

    body <- runDB $ selectOne $ do
        x <- from $ table @DocBody
        where_ $ x ^. DocBodyPage ==. val pid
        return x
        
    (fw,et) <- generateFormPost $ formBody pid body

    products <- runDB $ select $ do
        x :& i <- from $ table @Product
            `innerJoin` table @Item `on` (\(x :& i) -> x ^. ProductItem ==. i ^. ItemId)
        where_ $ just (x ^. ProductDisplay) ==. val (entityKey <$> body)
        return (x,i)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBody
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/body/body")


formBody :: WebpageId -> Maybe (Entity DocBody) -> Form DocBody
formBody pid body extra = do

    (bgColorR,bgColorV) <- mopt colorField FieldSettings
        { fsLabel = SomeMessage MsgBackgroundColor
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docBodyBgColor . entityVal <$> body)
    
    (layoutR,layoutV) <- mopt (selectField layoutOptions) FieldSettings
        { fsLabel = SomeMessage MsgLayout
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docBodyLayout . entityVal <$> body)
    
    let r = DocBody pid <$> bgColorR <*> layoutR
    idIconPalette <- newIdent
    let w = $(widgetFile "data/body/form")
    return (r,w)
  where
      layoutOptions = optionsPairs [ (MsgTable,DisplayLayoutTable)
                                   , (MsgList,DisplayLayoutList)
                                   , (MsgCards,DisplayLayoutCards)
                                   ]
