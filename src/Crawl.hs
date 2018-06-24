{-# LANGUAGE OverloadedStrings #-}

module Crawl where

import CrawlTypes
import EitherT
import Fetch

import Control.Exception.Safe       (SomeException)
import Data.Monoid                  ((<>))
import Data.Time                    (getCurrentTime)
import Network.HTTP.Client

steps :: Manager -> (Url -> Bool) -> Step -> EitherT SomeException IO Finished
steps http urlCheck = go
    where
    go step = do
        e <- runStep http urlCheck step
        case e of
            Left step' -> go step'
            Right finished -> return finished

runStep :: Manager -> (Url -> Bool) -> Step -> EitherT SomeException IO (Either Step Finished)
runStep http urlCheck (Step nextUrl cookies history) = do

    check (urlCheck nextUrl)

    res <- do

        req <- parse nextUrl

        httpFetch http req { redirectCount = 0,
                             proxy = (Just $ Proxy "127.0.0.1" 8080) }

    now <- liftTry getCurrentTime

    let cookies' = evictExpiredCookies (responseCookieJar res <> cookies) now

        history' = nextUrl : history

    return $
        case getRedirect res of
            Nothing -> Right $ Finished cookies' history'
            Just redirectUrl -> Left $ Step redirectUrl cookies' history'
