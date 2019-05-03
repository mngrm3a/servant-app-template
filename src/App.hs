{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module App
    ( mkApplication
    ) where

import           Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import           Crypto.JOSE.JWK      (JWK)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)
import           Network.Wai          (Application)
import           Servant              ((:<|>) (..), (:>) (..), Context (..),
                                       Context (..), Handler, Proxy (..),
                                       ServerT, err401, hoistServerWithContext,
                                       serveWithContext)
import           Servant              (Get, JSON)
import           Servant.Auth.Server  (AuthResult (..), Cookie,
                                       CookieSettings (..), IsSecure (..),
                                       JWTSettings, defaultCookieSettings,
                                       defaultJWTSettings, generateKey,
                                       throwAll)

import           App.App              (App, AppContext (ctxJSONWebKey),
                                       defaultAppContext, runApp)
import           App.Auth             (AuthenticatedAPI, LoginAPI, LogoutAPI,
                                       loginHandler, logoutHandler)

type AppAPI auths        = PublicAPI
                      :<|> ProctectedAPI auths
type PublicAPI           = "login"  :> LoginAPI
type ProctectedAPI auths = AuthenticatedAPI auths :>
                         ( "logout" :> LogoutAPI
                      :<|> "hello"  :> Get '[JSON] String
                         )

serveAppAPI :: CookieSettings
            -> JWTSettings
            -> ServerT (AppAPI auths) App
serveAppAPI cookieS jwtS = loginHandler cookieS jwtS
                      :<|> protectedAPI cookieS

protectedAPI cookieS (Authenticated sd) =
         logoutHandler cookieS sd
    :<|> helloHandler
protectedAPI _       _                  = throwAll err401

mkApplication :: IO Application
mkApplication = (mkApplication' . defaultAppContext) <$> generateKey

mkApplication' :: AppContext
               -> Application
mkApplication' appCtx = serveWithContext apiProxy servantCtx
    $ hoistServerWithContext apiProxy ctxProxy (runApp appCtx)
    $ serveAppAPI cookieS jwtS
  where
    apiProxy   = Proxy :: Proxy (AppAPI '[Cookie])
    ctxProxy   = Proxy :: Proxy '[CookieSettings, JWTSettings]
    cookieS    = defaultCookieSettings { cookieIsSecure = NotSecure
                                       , cookieXsrfSetting = Nothing
                                       }
    jwtS       = defaultJWTSettings $ ctxJSONWebKey appCtx
    servantCtx = cookieS :. jwtS :. EmptyContext

helloHandler :: App String
helloHandler = return $ "Welcome ...\n"
