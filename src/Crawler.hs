module Crawler where

import CrawlTypes
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client  (Manager, Request, Response)

class Monad m => Crawler m where

    fetch      :: Request -> m (Response ByteString)

    parseUrl   :: Url -> m Request

    throw      :: String -> m ()
