{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import Crawler
import CrawlTypes
import IOCrawler
import Url

import qualified Data.Set  as S                 (empty)
import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.Connection           (TLSSettings (TLSSettingsSimple))
import System.Environment           (getArgs)

main :: IO ()
main = do

    [site] <- getArgs

    let (Just d) = domain (Url site)

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    return ()

accept :: String -> Url -> Bool
accept d url =
    case domain url of
        Just x -> x == d
        Nothing -> False

