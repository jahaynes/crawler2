{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import CrawlTypes
import EitherT
import Url

import qualified Data.Set  as S                 (empty)
import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.Connection           (TLSSettings (TLSSettingsSimple))
import System.Environment           (getArgs)

import Control.Monad.IO.Class
import Control.Exception.Safe    (handleAnyDeep, handleAny, MonadCatch, MonadThrow, SomeException, throw, throwString)
import Data.ByteString.Char8     (unpack)
import Data.ByteString.Lazy      (ByteString)
import Data.CaseInsensitive      (mk)
import Network.HTTP.Client       (Manager, Request, Response, httpLbs, parseRequest, responseStatus, responseHeaders)
import Network.HTTP.Types.Status (statusCode)
import Safe                      (headMay)

class Monad m => Crawler m where

    new         :: m Manager

    parse       :: Url -> m Request
    parseEither :: Url -> m (Either SomeException Request)

    fetch       :: Manager -> Request -> m (Response ByteString)
    fetchEither :: Manager -> Request -> m (Either SomeException (Response ByteString))

    checked     :: m a -> m (Either SomeException a)
    unchecked   :: m (Either SomeException a) -> m a

instance Crawler IO where

    new = newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    parse (Url u) = parseRequest u
    parseEither u = handleAny (pure . Left) (Right <$> parse u)

    fetch       http req = httpLbs req http
    fetchEither http req = handleAny (pure . Left) (Right <$> (fetch http req))

    checked ma = handleAny (pure . Left) (Right <$> ma)

    unchecked mea = do
        ea <- mea
        case ea of
            Left e -> throw e
            Right a -> pure a

wrap ma = handleAny (pure . Left) (Right <$> ma)

main = do

    x <- checked $ do
        man  <- new
        req  <- parse (Url "https://github.com")
        resp <- fetch man req
        pure resp

    print x

{-
main :: IO ()
main = do

    [site] <- getArgs

    let (Just d) = domain (Url site)

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    x <- runEitherT $ urls [Url site] S.empty http (accept d)

    case x of
        Left e -> error $ show e
        Right c -> do

            print $ map crawled_getHistory $ c

            --let bs = crawled_getContents c
            --L8.writeFile "./lol" bs

accept :: String -> Url -> Bool
accept d url =
    case domain url of
        Just x -> x == d
        Nothing -> False

-}