{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Departments
  ( getDeptsR, postDeptsR
  , getDeptR, postDeptR
  , getDeptNewR, getDeptEditR, postDeptDeleR
  ) where

import Control.Applicative ((<|>)) 
import Control.Monad (void, forM)

import Data.Bifunctor (bimap)
import qualified Data.List.Safe as LS (last)
import Data.Maybe (isJust)
import Data.Text (Text, pack)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (?.), (==.), (:&)((:&))
    , isNothing_, Value (unValue), leftJoin, on, just
    )
import Database.Persist (Entity (Entity), entityVal, insert_, replace, delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Widget, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (DeptsR, DeptR, DeptNewR, DeptEditR, DeptDeleR, EmplsR)
    , AppMessage
      ( MsgDepartments, MsgDepartment, MsgSave, MsgCancel, MsgAlreadyExists
      , MsgCode, MsgName, MsgRecordAdded, MsgInvalidFormData, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgProperties, MsgDivisions, MsgDele, MsgParentDepartment
      , MsgRecordDeleted, MsgNoDivisionsInDepartment, MsgPleaseAddIfNecessary
      , MsgRecordEdited, MsgRootDepartment, MsgEmployees
      )
    )

import Model
    ( Depts (Depts), msgSuccess, msgError
    , DeptId, Dept(Dept, deptCode, deptName, deptParent)
    , EntityField (DeptCode, DeptId, DeptParent, DeptName)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core.Handler (newIdent, getMessageRender, getMessages, addMessageI, redirect, lookupGetParams)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Core (Yesod(defaultLayout), SomeMessage (SomeMessage), MonadHandler (liftHandler))
import Yesod.Form.Fields (textField, selectField, optionsPairs)
import Yesod.Form.Functions (generateFormPost, checkM, mreq, runFormPost, mopt)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvRequired, fvInput, fvLabel)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postDeptDeleR :: DeptId -> Depts -> Handler Html
postDeptDeleR did ps = do
    ((fr,_),_) <- runFormPost formDeptDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DeptsR ps
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DeptR did ps


getDeptEditR :: DeptId -> Depts -> Handler Html
getDeptEditR did ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    dept <- runDB $ selectOne $ do
        x <- from $ table @Dept
        where_ $ x ^. DeptId ==. val did
        return x

    (fw,et) <- generateFormPost $ formDepartment Nothing dept
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        $(widgetFile "data/depts/edit")


getDeptNewR :: Depts -> Handler Html
getDeptNewR ps@(Depts dids) = do

    (fw,et) <- generateFormPost $ formDepartment (LS.last dids) Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        $(widgetFile "data/depts/new")


formDepartment :: Maybe DeptId -> Maybe (Entity Dept) -> Form Dept
formDepartment did dept extra = do

    (codeR,codeV) <- mreq uniqueCodeField FieldSettings
        { fsLabel = SomeMessage MsgCode
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (deptCode . entityVal <$> dept)

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (deptName . entityVal <$> dept)

    deptOptions <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Dept
        orderBy [asc (x ^. DeptName)]
        return (x ^. DeptName, x ^. DeptId) )

    (parentR,parentV) <- mopt (selectField (optionsPairs deptOptions)) FieldSettings
        { fsLabel = SomeMessage MsgParentDepartment
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((deptParent . entityVal <$> dept) <|> pure did)

    let r = Dept <$> codeR <*> nameR <*> parentR

    let w = $(widgetFile "data/depts/form")
    return (r,w)
  where
      uniqueCodeField :: Field Handler Text
      uniqueCodeField = checkM uniqueCode textField

      uniqueCode :: Text -> Handler (Either AppMessage Text)
      uniqueCode code = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Dept
              where_ $ x ^. DeptCode ==. val code
              return x
          return $ case x of
            Nothing -> Right code
            Just (Entity did' _) -> case dept of
              Nothing -> Left MsgAlreadyExists
              Just (Entity did'' _) | did' == did'' -> Right code
                                    | otherwise -> Left MsgAlreadyExists


postDeptR :: DeptId -> Depts -> Handler Html
postDeptR did ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids

    dept <- runDB $ selectOne $ do
        x <- from $ table @Dept
        where_ $ x ^. DeptId ==. val did
        return x

    ((fr,fw),et) <- runFormPost $ formDepartment Nothing dept
    case fr of
      FormSuccess r -> do
          runDB $ replace did r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DeptR did ps
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgDepartment
              idOverlay <- newIdent
              $(widgetFile "data/depts/edit")


getDeptR :: DeptId -> Depts -> Handler Html
getDeptR did ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> did : dids
    
    dept <- runDB $ selectOne $ do
        x :& p <- from $ table @Dept
            `leftJoin` table @Dept `on` (\(x :& p) -> x ^. DeptParent ==. p ?. DeptId)
        where_ $ x ^. DeptId ==. val did
        return (x, p)

    (fw0,et0) <- generateFormPost formDeptDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartment
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/depts/dept") 


formDeptDelete :: Form ()
formDeptDelete extra = return (pure (), [whamlet|#{extra}|])


postDeptsR :: Depts -> Handler Html
postDeptsR ps = do

    ((fr,fw),et) <- runFormPost $ formDepartment Nothing Nothing

    case fr of
      FormSuccess r -> do
          void $ runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DeptsR ps

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/depts/new")


getDeptsR :: Depts -> Handler Html
getDeptsR ps@(Depts []) = do 

    open <- lookupGetParams "o"
    
    trees <- fetchDepts Nothing
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartments
        idOverlay <- newIdent
        $(widgetFile "data/depts/depts")
        
getDeptsR ps@(Depts dids) = do

    let open = ("o",) . pack . show . fromSqlKey <$> dids 

    depts <- runDB $ select $ do
        x <- from $ table @Dept
        where_ $ x ^. DeptParent ==. val (LS.last dids)
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDepartments
        idOverlay <- newIdent
        $(widgetFile "data/depts/divs")


newtype DeptTree = DeptTree [(Entity Dept, DeptTree)]


buildSnippet :: [Text] -> Maybe DeptId -> Depts -> DeptTree -> Widget
buildSnippet open msid ps@(Depts dids) (DeptTree trees) =
    [whamlet|
      <div>
        $forall (Entity did (Dept code name _),trees@(DeptTree subdepts)) <- trees
          $with (pid,level) <- (pack $ show $ fromSqlKey did,length dids + 1)
            $if (length subdepts) > 0
              <hr>
              <details #details#{pid} :elem pid open:open
                ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'collapse_all' : 'expand_all'"">
                <summary.row.surface-container>
                  <i.expand.padding.circle.border.wave style="margin-left:#{level}rem">
                    expand_all
                  <a.row.max.padding.wave href=@{DataR $ DeptR did ps}>
                    <div.max>
                      <h6.small>#{name}
                      <label>#{code}

                    <i>arrow_forward_ios

                ^{buildSnippet open msid (Depts (dids <> [did])) trees}

            $else
              <hr>
              <a.row.max.padding.surface-container.wave href=@{DataR $ DeptR did ps}>
                <i.expand.padding>
                <div.max>
                  <h6.small>#{name}
                  <label>#{code}
                <i>arrow_forward_ios
            |]


fetchDepts :: Maybe DeptId -> Handler DeptTree
fetchDepts pid = do
    depts <- runDB ( select $ do
        x <- from $ table @Dept
        where_ $ case pid of
          Nothing -> isNothing_ $ x ^. DeptParent
          Just did -> x ^. DeptParent ==. just (val did)
        orderBy [asc (x ^. DeptId)]
        return x )

    DeptTree <$> forM depts ( \p@(Entity did _) -> (p,) <$> fetchDepts (Just did) )
