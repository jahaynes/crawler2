{-# LANGUAGE BangPatterns,
             OverloadedStrings #-}

module Crawl where

import Crawler
import CrawlTypes
import Fetch
import Scrape

import Control.Exception.Safe       (SomeException)
import Data.Monoid                  ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time                    (getCurrentTime)

--Too concrete:
import Network.HTTP.Client          (CookieJar, Proxy (Proxy), Manager, evictExpiredCookies, 
                                     proxy, redirectCount, responseBody, responseCookieJar)

data Step = Step
          { step_getNextUrl :: Url
          , step_getCookies :: CookieJar
          , step_getHistory :: [Url]
          } deriving Show

seed :: String -> Step
seed url = Step (Url url) mempty []

seedu :: Url -> Step
seedu url = Step url mempty []

urls :: Crawler m => [Url] -> Set Url -> Manager -> (Url -> Bool) -> m [Crawled]
urls      []   !_    _        _ = return []
urls (u:rls) done http urlCheck
    | u `S.member` done = urls rls done http urlCheck
    | otherwise = do

        x <- steps http urlCheck (seedu u)

        let crawledUrl = head . crawled_getHistory $ x
            crawledPage = crawled_getContents $ x
            urls' = filter urlCheck . scrape crawledUrl $ crawledPage

        undefined -- liftTry $ print $ length urls'

        urls (urls' ++ rls) (S.insert u done) http urlCheck

    where
    steps :: Crawler m => Manager -> (Url -> Bool) -> Step -> m Crawled
    steps http urlCheck = go
        where
        go step = do
            e <- runStep http urlCheck step
            case e of
                Left step' -> go step'
                Right finished -> return finished

        runStep :: Crawler m => Manager -> (Url -> Bool) -> Step -> m (Either Step Crawled)
        runStep http urlCheck (Step nextUrl cookies history) = do

            check (urlCheck nextUrl)

            res <- do

                req <- parseUrl nextUrl

                httpFetch http req { redirectCount = 0,
                                    proxy = (Just $ Proxy "127.0.0.1" 8080) }

            now <- undefined -- liftTry getCurrentTime

            let cookies' = evictExpiredCookies (responseCookieJar res <> cookies) now

                history' = nextUrl : history

                contents = responseBody res

            return $
                case getRedirect res of
                    Nothing -> Right $ Crawled cookies' history' contents
                    Just redirectUrl -> Left $ Step redirectUrl cookies' history'
