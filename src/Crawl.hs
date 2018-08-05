{-# LANGUAGE BangPatterns,
             OverloadedStrings #-}

module Crawl where

import CrawlTypes
import EitherT
import Fetch
import Parse

import Control.Exception.Safe       (SomeException)
import Data.Monoid                  ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time                    (getCurrentTime)
import Network.HTTP.Client

data Step = Step
          { step_getNextUrl :: Url
          , step_getCookies :: CookieJar
          , step_getHistory :: [Url]
          } deriving Show

seed :: String -> Step
seed url = Step (Url url) mempty []

seedu :: Url -> Step
seedu url = Step url mempty []

urls :: [Url] -> Set Url -> Manager -> (Url -> Bool) -> EitherT SomeException IO [Crawled]
urls      []   !_    _        _ = return []
urls (u:rls) done http urlCheck
    | u `S.member` done = urls rls done http urlCheck
    | otherwise = do

        x <- steps http urlCheck (seedu u)

        let crawledUrl = head . crawled_getHistory $ x
            crawledPage = crawled_getContents $ x
            urls' = filter urlCheck . scrape crawledUrl $ crawledPage

        liftTry $ print $ length urls'

        urls (urls' ++ rls) (S.insert u done) http urlCheck

steps :: Manager -> (Url -> Bool) -> Step -> EitherT SomeException IO Crawled
steps http urlCheck = go
    where
    go step = do
        e <- runStep http urlCheck step
        case e of
            Left step' -> go step'
            Right finished -> return finished

runStep :: Manager -> (Url -> Bool) -> Step -> EitherT SomeException IO (Either Step Crawled)
runStep http urlCheck (Step nextUrl cookies history) = do

    check (urlCheck nextUrl)

    res <- do

        req <- parse nextUrl

        httpFetch http req { redirectCount = 0,
                             proxy = (Just $ Proxy "127.0.0.1" 8080) }

    now <- liftTry getCurrentTime

    let cookies' = evictExpiredCookies (responseCookieJar res <> cookies) now

        history' = nextUrl : history

        contents = responseBody res

    return $
        case getRedirect res of
            Nothing -> Right $ Crawled cookies' history' contents
            Just redirectUrl -> Left $ Step redirectUrl cookies' history'
