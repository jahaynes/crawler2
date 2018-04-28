{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CaseInsensitive         (mk)
import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.HTTP.Types.Status    (statusCode)
import Network.Connection           (TLSSettings (TLSSettingsSimple))
import Safe                         (headMay)

import Data.Time

import qualified Data.ByteString.Char8 as C8

type Url = String

data CrawlerStep = CrawlerStep
                 { crawlerStep_getNextUrl :: Url
                 , crawlerStep_getCookies :: [Cookie]
                 , crawlerStep_getHistory :: [Url]
                 } deriving Show
                               
data CrawlerStepFinished = CrawlerStepFinished
                         { crawlerStepFinished_getCookies :: [Cookie]
                         , crawlerStepFinished_getHistory :: [Url]
                         } deriving Show

seed :: Url -> CrawlerStep
seed url = CrawlerStep url [] []

main :: IO ()
main = do

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings
    
    let state1 = seed "https://www.google.com"
    
    print state1
    state2 <- runStep http (\_ -> pure True) state1

    case state2 of
        Left cont -> do
            x <- runStep http (\_ -> pure True) cont
            
            return ()
        Right done -> print "Done"

runStep :: Manager -> (Url -> IO Bool) -> CrawlerStep -> IO (Either CrawlerStep CrawlerStepFinished)
runStep http urlCheck (CrawlerStep nextUrl cookies history) = do
    req <- parseRequest nextUrl
    res <- httpLbs (req { redirectCount = 0
                        , proxy = Nothing -- Just (Proxy "127.0.0.1" 8080) 
                        } ) http

    now <- getCurrentTime
                        
    let cookieJar = evictExpiredCookies (responseCookieJar res) now
           
    putStrLn ""
    print cookieJar
    putStrLn ""
                        
        -- updateCookieJar
        -- check for cookies
        -- evictExpiredCookies
        -- check url pattern still ok

    return $ case getRedirect res of
                 Nothing -> Right $ CrawlerStepFinished cookies (nextUrl : history)
                 Just ru -> Left $ CrawlerStep ru cookies (nextUrl : history)
                                              
    where
    getRedirect :: Response a -> Maybe Url
    getRedirect res
        | code >= 300 && code < 400 = location res
        | otherwise = Nothing
        where
        code = statusCode . responseStatus $ res
        location = headMay
                 . map (C8.unpack . snd)
                 . filter (\x -> mk (fst x) == mk "Location")
                 . responseHeaders
