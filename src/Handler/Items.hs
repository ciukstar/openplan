{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Items
  ( getItemsR, postItemsR
  , getItemR, postItemR
  , getItemNewR, getItemEditR, postItemDeleR
  , getItemPhotoR
  ) where

import Data.Maybe (isJust)
import Data.Text (Text) 

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc, selectOne, where_, val
    , (^.), (==.), (=.)
    , Value (unValue), update, set
    )
import Database.Persist
    ( Entity (Entity), entityVal, insert, insert_, upsert, replace, delete
    )
import qualified Database.Persist as P ((=.))


import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR (ItemsR, ItemR, ItemNewR, ItemEditR, ItemDeleR, ItemPhotoR)
    , AppMessage
      ( MsgItems, MsgItem, MsgName, MsgDescription, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgCancel, MsgDele, MsgSave, MsgRecordAdded
      , MsgRecordEdited, MsgInvalidFormData, MsgRecordDeleted, MsgPrice
      , MsgCurrency, MsgRating, MsgAttribution, MsgPhoto, MsgLink
      )
    )

import Model
    ( msgSuccess, msgError
    , ItemId
    , Item (Item, itemName, itemDescr, itemPrice, itemCurrency, itemRating, itemLink)
    , ItemPhoto (ItemPhoto)
    , EntityField
      ( ItemName, ItemId, ItemPhotoItem, ItemPhotoAttribution, ItemPhotoMime
      , ItemPhotoPhoto
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_category_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Yesod.Core
    ( Yesod(defaultLayout), getMessageRender, FileInfo (fileContentType)
    , MonadHandler (liftHandler), TypedContent (TypedContent)
    , ToContent (toContent), fileSourceByteString
    )
import Yesod.Core.Handler
    (getMessages, newIdent, addMessageI, redirect)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields
    ( textField, textareaField, selectField, optionsPairs, doubleField, fileField
    , htmlField, urlField
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FormResult (FormSuccess)
    , fvInput, fvLabel, fvErrors, fvRequired, fvId
    )
import Yesod.Persist.Core (runDB)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (void)


postItemDeleR :: ItemId -> Handler Html
postItemDeleR sid = do
    ((fr,_),_) <- runFormPost formItemDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR ItemsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ ItemR sid


formItemDelete :: Form ()
formItemDelete extra = return (pure (), [whamlet|#{extra}|])


postItemR :: ItemId -> Handler Html
postItemR iid = do

    item <- runDB $ selectOne $ do
        x <- from $ table @Item
        where_ $ x ^. ItemId ==. val iid
        return x

    ((fr,fw),et) <- runFormPost $ formItem item      

    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          void $ runDB $ replace iid r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (ItemPhoto iid (fileContentType fi) bs attrib)
              [ ItemPhotoMime P.=. fileContentType fi
              , ItemPhotoPhoto P.=. bs
              , ItemPhotoAttribution P.=. attrib
              ]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ItemR iid
      FormSuccess (r,(Nothing,attrib)) -> do
          runDB $ replace iid r
          runDB $ update $ \x -> do
                set x [ ItemPhotoAttribution =. val attrib ]
                where_ $ x ^. ItemPhotoItem ==. val iid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ItemR iid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/items/edit")


getItemEditR :: ItemId -> Handler Html
getItemEditR iid = do

    item <- runDB $ selectOne $ do
        x <- from $ table @Item
        where_ $ x ^. ItemId ==. val iid
        return x

    (fw,et) <- generateFormPost $ formItem item

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/items/edit")


postItemsR :: Handler Html
postItemsR = do

    ((fr,fw),et) <- runFormPost $ formItem Nothing

    case fr of
      FormSuccess (r,(Just fi,attrib)) -> do
          iid <- runDB $ insert r
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (ItemPhoto iid (fileContentType fi) bs attrib)
                    [ ItemPhotoMime P.=. fileContentType fi
                    , ItemPhotoPhoto P.=. bs
                    , ItemPhotoAttribution P.=. attrib
                    ]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ItemsR
          
      FormSuccess (r,_) -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ItemsR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/items/new")



getItemNewR :: Handler Html
getItemNewR = do

    (fw,et) <- generateFormPost $ formItem Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/items/new")


formItem :: Maybe (Entity Item) -> Form (Item,(Maybe FileInfo,Maybe Html))
formItem item extra = do

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemName . entityVal <$> item)

    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemDescr . entityVal <$> item)

    (priceR,priceV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemPrice . entityVal <$> item)

    (currencyR,currencyV) <- mreq (selectField currencyOptions) FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemCurrency . entityVal <$> item)

    (ratingR,ratingV) <- mopt (selectField ratingOptions) FieldSettings
        { fsLabel = SomeMessage MsgRating
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemRating . entityVal <$> item)

    (linkR,linkV) <- mopt urlField FieldSettings
        { fsLabel = SomeMessage MsgLink
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (itemLink . entityVal <$> item)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case item of
      Just (Entity eid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @ItemPhoto
          where_ $ x ^. ItemPhotoItem ==. val eid
          return $ x ^. ItemPhotoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = (,) <$> (Item <$> nameR <*> descrR <*> priceR <*> currencyR <*> ratingR <*> linkR)
                <*> ((,) <$> photoR <*> attribR)

    idPhotoContainer <- newIdent
    idFigurePhoto <- newIdent
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent
            
    let w = $(widgetFile "data/items/form")
    return (r,w)
    
  where
      
      ratingOptions = optionsPairs [(T.replicate n "☆",n) | n <- [1..5]]
      
      currencyOptions = optionsPairs [("RUB" :: Text,"rub"),("USD","usd"),("EUR","eur")]



getItemR :: ItemId -> Handler Html
getItemR iid = do

    item <- runDB $ selectOne $ do
        x <- from $ table @Item
        where_ $ x ^. ItemId ==. val iid
        return x

    (fw0,et0) <- generateFormPost formItemDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/items/item")
    

getItemsR :: Handler Html
getItemsR = do

    items <- runDB $ select $ do
        x <- from $ table @Item
        orderBy [asc (x ^. ItemName)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        idOverlay <- newIdent
        $(widgetFile "data/items/items")



getItemPhotoR :: ItemId -> Handler TypedContent
getItemPhotoR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @ItemPhoto
        where_ $ x ^. ItemPhotoItem ==. val eid
        return x
    case photo of
      Just (Entity _ (ItemPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_category_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
