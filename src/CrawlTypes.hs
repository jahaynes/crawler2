module CrawlTypes where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client  (CookieJar)

newtype Url = Url String deriving Show

data Crawled = Crawled
             { crawled_getCookies  :: CookieJar
             , crawled_getHistory  :: [Url]
             , crawled_getContents :: ByteString
             } deriving Show
