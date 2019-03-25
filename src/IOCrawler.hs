{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, InstanceSigs #-}

module IOCrawler where

import Crawler
import CrawlTypes
import Url

import Network.HTTP.Client (Manager, Request, Response, httpLbs)

-- Probably to become a monadIO instead of monad
instance Crawler IO where

    --fetch :: Url -> Manager -> IO (Response b)
    fetch = httpLbs

    parseUrl :: Url -> IO Request
    parseUrl = undefined

    throw = error
