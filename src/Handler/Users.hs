{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Users
  ( getUsersR
  , getUserPhotoR
  , postUserDeleR
  , getUserEditR
  , getUserNewR
  , postUserR 
  , getUserR
  ) where

import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val
    , (^.), (==.)
    )
    
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR (UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR)
    , AppMessage
      ( MsgUsers, MsgPhoto, MsgUser, MsgAdministrator
      )
    )
    
import Model
    ( User(User), UserId
    , UserPhoto (UserPhoto)
    , EntityField (UserPhotoUser)
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = undefined


getUserEditR :: UserId -> Handler Html
getUserEditR uid = undefined


getUserNewR :: Handler Html
getUserNewR = undefined


postUserR :: UserId -> Handler Html
postUserR uid = undefined


getUserR :: UserId -> Handler Html
getUserR uid = undefined


getUsersR :: Handler Html
getUsersR = do

    users <- runDB $ select $ do
        x <- from $ table @User
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUsers
        idOverlay <- newIdent
        $(widgetFile "data/users/users")


getUserPhotoR :: UserId -> Handler TypedContent
getUserPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
