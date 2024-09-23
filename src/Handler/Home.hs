{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  ) where

import Database.Esqueleto.Experimental
    (SqlExpr, Value (unValue), selectOne, from, table, countRows)

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route(AuthR, DataR)
    , DataR (PrjsR)
    , AppMessage
      ( MsgAppName, MsgHome, MsgNumberOfProjects, MsgNumberOfTasks, MsgWelcomeTo
      , MsgSignIn, MsgLoginforMoreDetailsPlease, MsgProjectStatistics, MsgShowProjects
      )
    )
    
import Model (Prj, Task)
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Handler
    ( getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    
    nprjs <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Prj
        return (countRows :: SqlExpr (Value Int)) )

    ntasks <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Task
        return (countRows :: SqlExpr (Value Int)) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome
        idOverlay <- newIdent
        $(widgetFile "homepage")
