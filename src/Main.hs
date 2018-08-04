{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import EitherT

import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.Connection           (TLSSettings (TLSSettingsSimple))
import System.Environment           (getArgs)

main :: IO ()
main = do

    [site] <- getArgs

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    x <- runEitherT $ steps http (const True) (seed site)

    print x

