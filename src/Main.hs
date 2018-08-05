{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import CrawlTypes
import EitherT

import qualified Data.Set  as S                 (empty)
import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.Connection           (TLSSettings (TLSSettingsSimple))
import System.Environment           (getArgs)

main :: IO ()
main = do

    [site] <- getArgs

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    x <- runEitherT $ urls [Url site] S.empty http (const True)

    case x of
        Left e -> error $ show e
        Right c -> do

            print $ map crawled_getHistory $ c

            --let bs = crawled_getContents c
            --L8.writeFile "./lol" bs
