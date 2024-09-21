{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userAdmin)
    , Site (Site, siteName, siteDescr, siteHome)
    , Webpage (Webpage, webpageTitle, webpageBgColor, webpageSite)
    , Item (itemName, itemDescr, itemPrice, itemCurrency, itemRating, itemLink, Item)
    , ItemPhoto (itemPhotoItem, ItemPhoto, itemPhotoMime, itemPhotoPhoto, itemPhotoAttribution)
    , DocHeader
      ( DocHeader, docHeaderPage, docHeaderContentsType, docHeaderContents
      , docHeaderLevel, docHeaderLang, docHeaderCountry, docHeaderColor, docHeaderBgColor
      )
    , ContentsType (ContentsTypeText)
    , HeadingLevel (HeadingLevelH1)
    , DocBody (DocBody, docBodyPage, docBodyBgColor, docBodyLayout)
    , DisplayLayout (DisplayLayoutTable)
    , Product (Product, productDisplay, productItem)
    , Logo (Logo, logoHeader, logoPhoto, logoMime, logoAttribution)
    , Favicon (Favicon, faviconSite, faviconMime, faviconPhoto, faviconAttribution), UserPhoto (UserPhoto, userPhotoUser, userPhotoMime, userPhotoPhoto, userPhotoAttribution)
    )
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import qualified Data.ByteString as BS


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "marylopez"
    uid1 <- insert $ User { userEmail = "marylopez@xmail.edu"
                          , userPassword = Just pass1
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "jjohnson"
    uid2 <- insert $ User { userEmail = "jjohnson@xmail.edu"
                          , userPassword = Just pass2
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "jmaulsby"
    uid3 <- insert $ User { userEmail = "jmaulsby@xmail.edu"
                          , userPassword = Just pass3
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

            

    item1 <- insert $ Item { itemName = "Boots"
                           , itemDescr = Nothing
                           , itemPrice = 10
                           , itemCurrency = "usd"
                           , itemRating = Just 5
                           , itemLink = Just "https://www.google.com/search?q=boots&newwindow=1&sca_esv=d0c9352aa5174606&sca_upv=1&udm=2&sxsrf=ADLYWIKsKqVyh8lU5t_ZkIz6OfvOuHw2LQ:1726700988986&source=lnms&fbs=AEQNm0BNfNUooeQ9zNOHfD5dM-cuenhkgmBhnYUHrz76pe-e8wZDv0RMPHSRkHCNsZbIP5HzHYUD1XUTm-HLqxdFxra0L7ZkGto12nYXFok_FFPnEp1mJMBgsJboGp1vcvDK6HJsWCHzVgb0MD9onKVKCLcfkz3j3O3efOUHtwoipmPXsfidnf4&sa=X&ved=2ahUKEwiVrNO4zs2IAxWPRPEDHZlLAT0Q_AUoAnoECAQQBA&biw=1536&bih=744&dpr=1.25"
                           }
             
    liftIO (BS.readFile "demo/item_1.avif") >>= \bs ->
      insert_ ItemPhoto { itemPhotoItem = item1
                        , itemPhotoMime = "image/avif"
                        , itemPhotoPhoto = bs
                        , itemPhotoAttribution = Just freepik
                        }

    item2 <- insert $ Item { itemName = "Shirts"
                           , itemDescr = Nothing
                           , itemPrice = 20.89
                           , itemCurrency = "usd"
                           , itemRating = Just 5
                           , itemLink = Just "https://www.google.com/search?q=shirts&newwindow=1&sca_esv=d0c9352aa5174606&sca_upv=1&udm=2&biw=1536&bih=744&sxsrf=ADLYWILaj7Jh_HmBRUhl2wy96h7y86EOHQ%3A1726700991047&ei=v13rZsPJAruQxc8PieXGwAg&ved=0ahUKEwiDpdG5zs2IAxU7SPEDHYmyEYgQ4dUDCBA&uact=5&oq=shirts&gs_lp=Egxnd3Mtd2l6LXNlcnAiBnNoaXJ0czIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgAQyBRAAGIAEMgUQABiABDIFEAAYgARIgiZQwA5YtR1wAXgAkAEAmAFioAGzBKoBATa4AQPIAQD4AQGYAgegApIFwgIEECMYJ8ICChAAGIAEGEMYigWYAwCIBgGSBwMxLjagB7Ae&sclient=gws-wiz-serp"
                           }
             
    liftIO (BS.readFile "demo/item_2.avif") >>= \bs ->
      insert_ ItemPhoto { itemPhotoItem = item2
                        , itemPhotoMime = "image/avif"
                        , itemPhotoPhoto = bs
                        , itemPhotoAttribution = Just freepik
                        }


    sid1 <- insert $ Site { siteName = "Site #1"
                          , siteDescr = Nothing
                          , siteHome = Nothing
                          }
             
    liftIO (BS.readFile "demo/favicon_1.ico") >>= \bs ->
      insert_ Favicon { faviconSite = sid1
                      , faviconMime = "image/x-icon"
                      , faviconPhoto = bs
                      , faviconAttribution = Nothing
                      }

    sid2 <- insert $ Site { siteName = "Site #2"
                          , siteDescr = Nothing
                          , siteHome = Nothing
                          }


    pid11 <- insert $ Webpage { webpageSite = sid1
                              , webpageTitle = "Sales #1"
                              , webpageBgColor = Just "#0000ff"
                              }

    h111 <- insert $ DocHeader { docHeaderPage = pid11
                               , docHeaderContentsType = ContentsTypeText
                               , docHeaderContents = Just "Marketplace #1"
                               , docHeaderLevel = Just HeadingLevelH1
                               , docHeaderLang = Just "en"
                               , docHeaderCountry = Just "US"
                               , docHeaderColor = Just "#ff0000"
                               , docHeaderBgColor = Just "#FFEA00"
                               }

    liftIO (BS.readFile "demo/local_convenience_store_24dp_013048_FILL0_wght400_GRAD0_opsz24.svg") >>= \bs ->
        insert_ $ Logo { logoHeader = h111
                       , logoPhoto = bs
                       , logoMime = "image/svg+xml"
                       , logoAttribution = Nothing
                       }

    b111 <- insert $ DocBody { docBodyPage = pid11
                             , docBodyBgColor = Just "white"
                             , docBodyLayout = Just DisplayLayoutTable
                             }

    insert_ $ Product { productItem = item1
                      , productDisplay = b111
                      }

    insert_ $ Product { productItem = item2
                      , productDisplay = b111
                      }


    pid12 <- insert $ Webpage { webpageSite = sid1
                              , webpageTitle = "Sales #2"
                              , webpageBgColor = Just "#00ff00"
                              }
    

    return ()
