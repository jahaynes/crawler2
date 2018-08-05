{-# LANGUAGE OverloadedStrings #-}

module Parse where

import CrawlTypes

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8              as L8
import           Data.Maybe
import           Data.Set                   (fromList, toList)
import           Text.HTML.TagSoup
import           Network.URI

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

parseUrl :: String -> String -> Maybe Url
parseUrl base c =
    case parseAbsoluteURI c of
        Just absUrl -> return (unpack absUrl)
        Nothing ->
            case parseRelativeReference c of
                Nothing -> error $ show c
                Just relRef -> return . unpack . relativeTo relRef $ (fromJust $ parseURI base)
    where
    unpack x =
        let noFrag = x { uriFragment = "" }
        in Url . uriToString id noFrag $ ""
