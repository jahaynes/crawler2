{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crawl
import EitherT
import Fetch

import Network.HTTP.Client          
import Network.HTTP.Client.TLS      (mkManagerSettings)
import Network.Connection           (TLSSettings (TLSSettingsSimple))

main :: IO ()
main = do

    http <- newManager (mkManagerSettings (TLSSettingsSimple True False False) Nothing) -- tlsManagerSettings

    x <- runEitherT $ steps http (const True) (seed "site")

    print x

