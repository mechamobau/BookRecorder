{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Lucius          (luciusFile)
import Text.Hamlet          (hamletFile)

import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

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

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

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
type Form x = Html -> MForm (HandlerFor App)
    (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    -- isAuthorized ProfileR _ = isAuthenticated

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod

        muser <- lookupSession "_ID"
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Dashboard"
                    , menuItemRoute = DashboardR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            toWidgetHead $(luciusFile "templates/default-layout-wrapper.lucius")
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (LoginR) = return ("Login", Just HomeR)
    breadcrumb (UserR) = return ("Cadastro", Just HomeR)
    breadcrumb  _ = return ("home", Nothing)

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master    

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    msession <- lookupSession "_ID"
    return $ case msession of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
