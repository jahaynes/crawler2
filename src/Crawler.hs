module Crawler where

import CrawlTypes
import Network.HTTP.Client (Request, Response)

class Monad m => Crawler m where

    fetch      :: Url -> m (Response b)

    parseUrl   :: Url -> m Request

    throw      :: String -> m ()
