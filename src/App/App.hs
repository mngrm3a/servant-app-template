{-# LANGUAGE DeriveGeneric #-}

module App.App
    ( App
    , AppContext(..)
    , runApp
    , defaultAppContext
    , throwError
    , liftIO
    , liftHandler
    , liftIOEither
    , liftIOMaybe
    ) where

import           Control.Monad.Except   (ExceptT (..), throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT (..), ask, asks, runReaderT)
import           Crypto.JOSE.JWK        (JWK)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Servant                (Handler (..), ServantErr)

type App = ReaderT AppContext Handler

runApp :: AppContext
       -> App a
       -> Handler a
runApp = flip runReaderT

data AppContext = AppContext
                { ctxJSONWebKey :: JWK
                }
    deriving (Show, Eq, Generic)

instance FromJSON AppContext
instance ToJSON AppContext

defaultAppContext :: JWK -> AppContext
defaultAppContext = AppContext

liftHandler :: Handler a
            -> App a
liftHandler = ReaderT . const

liftIOEither :: IO (Either ServantErr a)
             -> App a
liftIOEither = liftHandler . Handler . ExceptT

liftIOMaybe :: ServantErr
            -> IO (Maybe a)
            -> App a
liftIOMaybe e = liftIOEither . fmap (maybe (Left e) Right)
