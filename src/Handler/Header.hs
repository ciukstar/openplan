{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Header
  ( getHeaderR, postHeaderR
  , getHeaderLogoR
  ) where


import Control.Monad (void)

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    (selectOne, from, table, Value (unValue), where_, val
    , (^.), (==.), (=.)
    , update, set
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey
    )
import qualified Database.Persist as P
    ( upsertBy, upsert, (=.)
    )

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, PageR, StaticR)
    , DataR (WebpageR, WebpagesR, HeaderR, BodyR, HeaderLogoR)
    , AppMessage
      ( MsgHeader, MsgText, MsgHtml, MsgCancel, MsgSave, MsgSettings, MsgBody
      , MsgFooter, MsgContentType, MsgContents, MsgHeadingLevel, MsgRecordEdited
      , MsgPage, MsgLangEn, MsgLanguage, MsgLangRu, MsgCountryUS, MsgCountryRU
      , MsgCountry, MsgColor, MsgBackgroundColor, MsgLogo, MsgAttribution
      )
    )

import Model
    ( msgSuccess
    , ContentsType (ContentsTypeText, ContentsTypeHtml)
    , SiteId, WebpageId, DocHeaderId
    , DocHeader
      ( DocHeader, docHeaderContentsType, docHeaderContents, docHeaderLevel
      , docHeaderLang, docHeaderColor, docHeaderBgColor
      )
    , HeadingLevel
      ( HeadingLevelH1, HeadingLevelH2, HeadingLevelH3, HeadingLevelH4
      , HeadingLevelH5, HeadingLevelH6
      )
    , Unique (UniqueDocHeader)
    , Logo (Logo)
    , EntityField
      ( DocHeaderContentsType, DocHeaderContents, DocHeaderLevel, DocHeaderLang
      , DocHeaderCountry, DocHeaderColor, DocHeaderBgColor, LogoHeader
      , LogoAttribution, LogoMime, LogoPhoto, DocHeaderPage
      )
    )

import Settings.StaticFiles
    ( img_branding_watermark_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_image_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    )
    
import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, SomeMessage (SomeMessage), addMessageI
    , redirect, getMessages, newIdent, getMessageRender, MonadHandler (liftHandler)
    , FileInfo (fileContentType), TypedContent (TypedContent), ToContent (toContent)
    , fileSourceByteString
    )
import Settings (widgetFile)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (selectField, optionsPairs, htmlField, colorField, fileField)
import Yesod.Form.Types
    ( FieldView (fvInput, fvLabel, fvErrors, fvRequired, fvId)
    , FieldSettings (fsLabel, fsTooltip, fsId, fsName, fsAttrs, FieldSettings)
    , FormResult (FormSuccess)
    )


postHeaderR :: SiteId -> WebpageId -> Handler Html
postHeaderR sid pid = do

    header <- runDB $ selectOne $ from $ table @DocHeader
    
    ((fr,fw),et) <- runFormPost $ formHeader pid header

    case fr of
      FormSuccess (r@(DocHeader _ ct html level lang country color bg),(Just fi,attrib)) -> do
          h <- runDB $ P.upsertBy (UniqueDocHeader pid) r
              [ DocHeaderContentsType P.=. ct
              , DocHeaderContents P.=. html
              , DocHeaderLevel P.=. level
              , DocHeaderLang P.=. lang
              , DocHeaderCountry P.=. country
              , DocHeaderColor P.=. color
              , DocHeaderBgColor P.=. bg
              ]
          bs <- fileSourceByteString fi
          void $ runDB $ P.upsert (Logo (entityKey h) (fileContentType fi) bs attrib)
              [ LogoMime P.=. fileContentType fi
              , LogoPhoto P.=. bs
              , LogoAttribution P.=. attrib
              ]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ HeaderR sid pid
          
      FormSuccess (r@(DocHeader _ ct html level lang country color bg),(Nothing,attrib)) -> do
          h <- runDB $ P.upsertBy (UniqueDocHeader pid) r
              [ DocHeaderContentsType P.=. ct
              , DocHeaderContents P.=. html
              , DocHeaderLevel P.=. level
              , DocHeaderLang P.=. lang
              , DocHeaderCountry P.=. country
              , DocHeaderColor P.=. color
              , DocHeaderBgColor P.=. bg
              ]
          runDB $ update $ \x -> do
                set x [ LogoAttribution =. val attrib ]
                where_ $ x ^. LogoHeader ==. val (entityKey h)
              
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ HeaderR sid pid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgHeader
              idOverlay <- newIdent
              idFormHeader <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/header/header")


getHeaderR :: SiteId -> WebpageId -> Handler Html
getHeaderR sid pid = do

    header <- runDB $ selectOne $ do
        x <- from $ table @DocHeader
        where_ $ x ^. DocHeaderPage ==. val pid
        return x
    
    (fw,et) <- generateFormPost $ formHeader pid header
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHeader
        idOverlay <- newIdent
        idFormHeader <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/header/header")


formHeader :: WebpageId -> Maybe (Entity DocHeader) -> Form (DocHeader,(Maybe FileInfo, Maybe Html))
formHeader pid header extra = do
    
    (typeR,typeV) <- mreq (selectField typeOptions) FieldSettings
        { fsLabel = SomeMessage MsgContentType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderContentsType . entityVal <$> header)
    
    (htmlR,htmlV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgContents
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderContents . entityVal <$> header)
    
    (levelR,levelV) <- mopt (selectField levelOptions) FieldSettings
        { fsLabel = SomeMessage MsgHeadingLevel
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderLevel . entityVal <$> header)
    
    (langR,langV) <- mopt (selectField langOptions) FieldSettings
        { fsLabel = SomeMessage MsgLanguage
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderLang . entityVal <$> header)
    
    (countryR,countryV) <- mopt (selectField countryOptions) FieldSettings
        { fsLabel = SomeMessage MsgCountry
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderLang . entityVal <$> header)

    (colorR,colorV) <- mopt colorField FieldSettings
        { fsLabel = SomeMessage MsgColor
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderColor . entityVal <$> header)

    (bgColorR,bgColorV) <- mopt colorField FieldSettings
        { fsLabel = SomeMessage MsgBackgroundColor
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (docHeaderBgColor . entityVal <$> header)

    (logoR,logoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgLogo
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case header of
      Just (Entity hid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @Logo
          where_ $ x ^. LogoHeader ==. val hid
          return $ x ^. LogoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)
    
    let r = (,) <$>
            ( DocHeader pid <$> typeR <*> htmlR <*> levelR <*> langR <*> countryR <*> colorR <*> bgColorR
            ) <*> ((,) <$> logoR <*> attribR)

    idLogoContainer <- newIdent
    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    idIconPalette <- newIdent
    idIconPaletteBg <- newIdent
    let w = $(widgetFile "data/header/form")
    
    return (r,w)
    
  where
      countryOptions = optionsPairs [(MsgCountryUS, "US"),(MsgCountryRU, "RU")]
      langOptions = optionsPairs [(MsgLangEn, "en"),(MsgLangRu, "ru")]

      levelOptions = optionsPairs [ ("h1" :: Text, HeadingLevelH1),("h2", HeadingLevelH2)
                                  , ("h3", HeadingLevelH3),("h4", HeadingLevelH4)
                                  , ("h5", HeadingLevelH5),("h6", HeadingLevelH6)
                                  ]
      
      typeOptions = optionsPairs [(MsgText, ContentsTypeText),(MsgHtml,ContentsTypeHtml)]
      

getHeaderLogoR :: DocHeaderId -> Handler TypedContent
getHeaderLogoR hid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Logo
        where_ $ x ^. LogoHeader ==. val hid
        return x
    case photo of
      Just (Entity _ (Logo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_branding_watermark_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
