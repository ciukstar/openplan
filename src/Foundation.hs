{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Foundation where

import Control.Monad.Logger (LogSource)

import Import.NoFoundation

import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.List.Safe as LS (head)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, (^.), select, just )
import qualified Database.Esqueleto.Experimental as E ((==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)


import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.Message
    ( AuthMessage(InvalidLogin), defaultMessage, englishMessage, russianMessage )
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }


mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a


msgTaskStatus :: TaskStatus -> AppMessage
msgTaskStatus TaskStatusNotStarted = MsgTaskStatusNotStarted
msgTaskStatus TaskStatusInProgress = MsgTaskStatusInProgress
msgTaskStatus TaskStatusPaused = MsgTaskStatusPaused
msgTaskStatus TaskStatusCompleted = MsgTaskStatusCompleted
msgTaskStatus TaskStatusUncompleted = MsgTaskStatusUncompleted
msgTaskStatus TaskStatusPartiallyCompleted = MsgTaskStatusPartiallyCompleted


widgetTopbar :: Maybe (Route App,[(Text,Text)]) -- ^ Back button
             -> Text                            -- ^ Title 
             -> Text                            -- ^ Overlay id
             -> Maybe Text                      -- ^ Id of delete dialog
             -> Maybe (Route App)               -- ^ Edit button
             -> Widget
widgetTopbar backlink title idOverlay idDialogDelete editRoute = do
    stati <- reqGetParams <$> getRequest
    rndr <- getUrlRenderParams
    idDialogMainMenu <- newIdent
    $(widgetFile "widgets/topbar")


widgetAccount :: Widget
widgetAccount = do
    user <- maybeAuth
    $(widgetFile "widgets/account")


widgetSnackbar :: [(Text,Html)] -> Widget
widgetSnackbar msgs = $(widgetFile "widgets/snackbar")


widgetMainMenuTrigger :: Text -> Text -> Widget
widgetMainMenuTrigger idOverlay idDialogMainMenu = $(widgetFile "widgets/trigger")
    

widgetMainMenu :: Text -> Text -> Widget
widgetMainMenu idOverlay idDialogMainMenu = do

    uid <- maybeAuthId
    
    empl  <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Empl
        where_ $ just (x ^. EmplUser) E.==. val uid
        return x
    
    curr <- getCurrentRoute
    idButtonMainMenuClose <- newIdent
    $(widgetFile "widgets/menu")



widgetTheme :: Widget
widgetTheme = $(widgetFile "widgets/theme")


widgetLang :: Route App -> Text -> Widget
widgetLang action backlink  = do
    
    lang <- fromMaybe "en" . LS.head <$> languages
    
    idMenuLang <- newIdent
    idHiddenSelect <- newIdent
    idFormLang <- newIdent
    idInputBacklink <- newIdent
    
    $(widgetFile "widgets/lang")


postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField paramLang
    back <- runInputPost $ ireq urlField paramBacklink
    setLanguage lang
    redirect back


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        lang <- fromMaybe "en" . LS.head <$> languages

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- addStylesheet $ StaticR css_bootstrap_css
                                    -- ^ generated from @Settings/StaticFiles.hs@
            $(widgetFile "default-layout")
          
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = setUltDestCurrent >> return Authorized
    isAuthorized DocsR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized PwdResetR _ = return Authorized
    isAuthorized LangR _ = return Authorized
    
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    
    isAuthorized (DataR (TaskStatusR eid _ _)) _ = setUltDestCurrent >> isEmployeeSelf eid
    isAuthorized (DataR (AdminTaskR eid _)) _ = setUltDestCurrent >> isEmployeeSelf eid
    isAuthorized (DataR (AdminTasksR eid)) _ = setUltDestCurrent >> isEmployeeSelf eid

    isAuthorized (DataR (TaskDeleR {})) _ = isAuthenticated
    isAuthorized (DataR (TaskEditR {})) _ = isAuthenticated
    isAuthorized (DataR (TaskNewR _ _)) _ = isAuthenticated
    isAuthorized (DataR (TaskR {})) _ = isAuthenticated
    isAuthorized (DataR (TasksR _ _)) _ = isAuthenticated

    
    isAuthorized (DataR (MonitorPrjTasksR eid _)) _ = isEmployeeSelf eid
    isAuthorized (DataR (MonitorPrjR eid _)) _ = isEmployeeSelf eid
    isAuthorized (DataR (MonitorR eid)) _ = setUltDestCurrent >> isEmployeeSelf eid
    isAuthorized (DataR (PrjTeamR _)) _ = isAuthenticated

    isAuthorized (DataR (PrjDeleR _)) _ = isAuthenticated
    isAuthorized (DataR (PrjEditR _)) _ = isAuthenticated
    isAuthorized (DataR PrjNewR) _ = isAuthenticated
    isAuthorized (DataR (PrjR _)) _ = isAuthenticated
    isAuthorized (DataR PrjsR) _ = setUltDestCurrent >> isAuthenticated

    isAuthorized (DataR (OutletDeleR _)) _ = isAuthenticated
    isAuthorized (DataR (OutletEditR _)) _ = isAuthenticated
    isAuthorized (DataR OutletNewR) _ = isAuthenticated
    isAuthorized (DataR (OutletR _)) _ = isAuthenticated
    isAuthorized (DataR OutletsR) _ = setUltDestCurrent >> isAuthenticated
    
    isAuthorized (DataR (EmplTasksR {})) _ = isAuthenticated
    isAuthorized (DataR (EmplProjectsR {})) _ = isAuthenticated

    isAuthorized (DataR (EmplDeleR {})) _ = isAuthenticated
    isAuthorized (DataR (EmplEditR {})) _ = isAuthenticated
    isAuthorized (DataR (EmplNewR _ _)) _ = isAuthenticated
    isAuthorized (DataR (EmplR {})) _ = isAuthenticated
    isAuthorized (DataR (EmplsR _ _)) _ = isAuthenticated

    isAuthorized (DataR EmployeePhotoR) _ = isAuthenticated

    isAuthorized (DataR (DeptDeleR _ _)) _ = isAuthenticated
    isAuthorized (DataR (DeptEditR _ _)) _ = isAuthenticated
    isAuthorized (DataR (DeptNewR _)) _ = isAuthenticated
    isAuthorized (DataR (DeptR _ _)) _ = isAuthenticated
    isAuthorized (DataR (DeptsR _)) _ = setUltDestCurrent >> isAuthenticated

    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR UserNewR) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized (DataR UsersR) _ = setUltDestCurrent >> isAdmin
    isAuthorized (DataR (UserPhotoR _)) _ = return Authorized
    

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    errorHandler NotFound = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPageNotFound
            idHeader <- newIdent
            idHeaderStart <- newIdent
            $(widgetFile "error/not-found")
        provideRep $ return $ object ["message" .= ("Page not found." :: Text)]
        provideRep $ return ("Page not found." :: Text)

    errorHandler (PermissionDenied msg) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPermissionDenied
            msgr <- getMessageRender
            idOverlay <- newIdent
            $(widgetFile "error/permission-denied")
        provideRep $ do
            msgr <- getMessageRender
            return $ object ["message" .= (msgr MsgPermissionDenied <> "Permission Denied. " <> msg)]
        provideRep $ return $ "Permission Denied. " <> msg

    errorHandler (InvalidArgs msgs) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgInvalidArguments
            idHeader <- newIdent
            $(widgetFile "error/invalid-args")
        provideRep $ return $ object ["message" .= msgs]
        provideRep $ return $ T.intercalate ", " msgs

    errorHandler x = defaultErrorHandler x


getPwdResetR :: Handler Html
getPwdResetR = do
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRestoreLogin
        $(widgetFile "auth/restore")


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate (Creds _plugin ident _extra) = liftHandler $ do
        user <- runDB $ selectOne $ do
            x <- from $ table @User
            where_ $ x ^. UserEmail E.==. val ident
            return x
        return $ case user of
                Just (Entity uid _) -> Authenticated uid
                Nothing -> UserError InvalidLogin

    authPlugins :: App -> [AuthPlugin App]
    authPlugins _app = [authHashDBWithForm formLogin (Just . UniqueUser)]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs
    

formLogin :: Route App -> Widget
formLogin route = do

    users <- liftHandler $ runDB $ select $ from $ table @User
    
    msgr <- getMessageRender
    msgs <- getMessages
    idOverlay <- newIdent
    idInputUsername <- newIdent
    idInputPassword <- newIdent
    $(widgetFile "auth/form")


isEmployeeSelf :: EmplId -> Handler AuthResult
isEmployeeSelf eid = do
    user <- maybeAuth
    case user of
        Just (Entity uid _) -> do
            empl <- runDB $ selectOne $ do
                x <- from $ table @Empl
                where_ $ x ^. EmplUser E.==. val uid
                return x
            case empl of
              Just (Entity eid' _) | eid' == eid -> return Authorized
                                   | otherwise -> unauthorizedI MsgNotAuthorized
              Nothing -> unauthorizedI MsgNotAuthorized
        Nothing -> unauthorizedI MsgLoginPlease


isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True)) -> return Authorized
        Just (Entity _ (User _ _ _ False)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgLoginPlease


isAdministrator :: Handler Bool
isAdministrator = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True)) -> return True
        Just (Entity _ (User _ _ _ False)) -> return False
        Nothing -> return False
    


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    case muid of
        Nothing -> unauthorizedI MsgLoginToAccessPlease
        Just _ -> return Authorized



instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage _ ("ru":_) = russianFormMessage
    renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
