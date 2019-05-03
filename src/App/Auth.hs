{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module App.Auth
    ( AuthenticatedAPI
    , LoginAPI
    , LogoutAPI
    , SessionData
    , loginHandler
    , logoutHandler
    ) where

import           App.App             (App, liftIO, throwError)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Servant             ((:>), Header, Headers, JSON,
                                      NoContent (..), PostNoContent, ReqBody,
                                      err401)
import           Servant.Auth.Server (Auth, CookieSettings, FromJWT,
                                      JWTSettings, SetCookie, ToJWT,
                                      acceptLogin, clearSession)

data AuthData = AuthData
              { name :: !Text
              }
    deriving (Read, Show, Eq, Generic)

instance FromJSON AuthData
instance ToJSON AuthData

data SessionData = SessionData
                 { sdUsername :: !Text
                 }
    deriving (Read, Show, Eq, Generic)

instance FromJSON SessionData
instance FromJWT SessionData
instance ToJSON SessionData
instance ToJWT SessionData

type AuthenticatedAPI auths = Auth auths SessionData
type LoginAPI               = ReqBody '[JSON] AuthData
                            :> PostNoContent '[JSON] AuthResponse
type LogoutAPI              = PostNoContent '[JSON] AuthResponse
type AuthResponse           = Headers '[CookieHeader, CookieHeader] NoContent
type CookieHeader           = Header "Set-Cookie" SetCookie

loginHandler :: CookieSettings
             -> JWTSettings
             -> AuthData
             -> App AuthResponse
loginHandler cookieS jwtS authData =
    (fmap (<*>)) (checkAuthData authData >>= liftIO . acceptLogin cookieS jwtS)
             <*> (pure $ pure NoContent)
             >>= maybe (throwError err401) return
  where
    checkAuthData :: AuthData -> App SessionData
    checkAuthData = return . SessionData . name

logoutHandler :: CookieSettings
              -> SessionData
              -> App AuthResponse
logoutHandler cookieS sessionData = return $ clearSession cookieS NoContent
