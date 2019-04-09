{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Crawler
import CrawlTypes

import Control.Exception.Safe    (MonadCatch, SomeException, throwString)
import Data.ByteString.Char8     (unpack)
import Data.ByteString.Lazy      (ByteString)
import Data.CaseInsensitive      (mk)
import Network.HTTP.Client       (Manager, Request, Response, httpLbs, parseRequest, responseStatus, responseHeaders)
import Network.HTTP.Types.Status (statusCode)
import Safe                      (headMay)

check :: Crawler m => Bool -> m ()
check True  = return ()
check False = throw "Unacceptable url"

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
