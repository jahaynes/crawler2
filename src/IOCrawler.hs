{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, InstanceSigs #-}

module IOCrawler where

import Crawler
import CrawlTypes
import Url
import Control.Monad.IO.Class 
import Data.ByteString.Lazy (ByteString)

import Network.HTTP.Client as C (Manager, Request, Response, httpLbs, parseUrl)

-- Probably to become a monadIO instead of monad
{-
instance Crawler IO where

    fetch :: Manager -> Request -> IO (Response ByteString)
    fetch http req = httpLbs req http

    parseUrl :: Url -> IO Request
    parseUrl (Url u) = C.parseUrl u

    throw :: String -> IO ()
    throw = error
        -}

data IOCrawler m = IOCrawler Manager m

instance Functor IOCrawler
instance Applicative IOCrawler
instance Monad IOCrawler
instance MonadIO IOCrawler

instance Crawler IOCrawler where

    fetch :: Request -> IOCrawler (Response ByteString)
    fetch req = do
        http <- liftIO $ ask
        liftIO $ httpLbs req undefined
