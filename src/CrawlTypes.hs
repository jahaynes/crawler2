module CrawlTypes where

import Network.HTTP.Client (CookieJar)

newtype Url = Url String deriving Show

data Step = Step
          { step_getNextUrl :: Url
          , step_getCookies :: CookieJar
          , step_getHistory :: [Url]
          } deriving Show

data Finished = Finished
              { finished_getCookies :: CookieJar
              , finished_getHistory :: [Url]
              } deriving Show
