{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import CrawlTypes
import EitherT
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

    x <- runEitherT $ urls [Url site] S.empty http (accept d)

    case x of
        Left e -> error $ show e
        Right c -> do

            print $ map crawled_getHistory $ c

            --let bs = crawled_getContents c
            --L8.writeFile "./lol" bs

accept :: String -> Url -> Bool
accept d url =
    case domain url of
        Just x -> x == d
        Nothing -> False

