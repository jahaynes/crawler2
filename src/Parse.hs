{-# LANGUAGE OverloadedStrings #-}

module Parse where

import CrawlTypes
import Url

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8              as L8
import           Data.Maybe                 (mapMaybe)
import           Data.Set                   (fromList, toList)
import           Text.HTML.TagSoup

scrape :: Url -> ByteString -> [Url]
scrape (Url baseUrl) page = do

    let tags = canonicalizeTags
             . parseTags
             $ page

    let interests = filter (isTagOpenName "a") tags

    toList . fromList
           . mapMaybe (parseUrl baseUrl . L8.unpack)
           . filter (not . L8.null)
           . map (fromAttrib "href")
           $ interests
