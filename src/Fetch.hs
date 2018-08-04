{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import CrawlTypes
import EitherT

import Control.Exception.Safe    (MonadCatch, SomeException, throwString)
import Data.ByteString.Char8     (unpack)
import Data.ByteString.Lazy      (ByteString)
import Data.CaseInsensitive      (mk)
import Network.HTTP.Client       (Manager, Request, Response, httpLbs, parseRequest, responseStatus, responseHeaders)
import Network.HTTP.Types.Status (statusCode)
import Safe                      (headMay)

parse :: MonadCatch m => Url -> EitherT SomeException m Request
parse (Url u) = liftTry (parseRequest u)

httpFetch :: Manager -> Request -> EitherT SomeException IO (Response ByteString)
httpFetch http req = liftTry (httpLbs req http)

check :: MonadCatch m => Bool -> EitherT SomeException m ()
check True  = return ()
check False = throw "Unacceptable url"

throw :: MonadCatch m => String -> EitherT SomeException m a
throw str = liftTry (throwString str)

getRedirect :: Response a -> Maybe Url
getRedirect res
    | code >= 300 && code < 400 = Url <$> location res
    | otherwise = Nothing
    where
    code = statusCode . responseStatus $ res
    location = headMay
             . map (unpack . snd)
             . filter (\x -> mk (fst x) == mk "Location")
             . responseHeaders
