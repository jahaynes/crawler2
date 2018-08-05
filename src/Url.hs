module Url where

import CrawlTypes          (Url (Url))

import Control.Applicative ((<|>))
import Control.Monad.Fail  (MonadFail, fail)
import Network.URI         (parseAbsoluteURI, parseRelativeReference, parseURI, relativeTo, uriFragment, uriToString)
import Prelude      hiding (fail)
import Text.Parsec         (many1, noneOf, parse, string, try)

parseUrl :: MonadFail m => String -> String -> m Url
parseUrl base c =
    case parseAbsoluteURI c of
        Just absUrl -> return (unpack absUrl)
        Nothing ->
            case parseRelativeReference c of
                Nothing -> fail "Could not parse url"
                Just relRef ->
                    case parseURI base of
                        Nothing -> fail "Could not parse base url"
                        Just bUrl -> return . unpack . relativeTo relRef $ bUrl
    where
    unpack x =
        let noFrag = x { uriFragment = "" }
        in Url . uriToString id noFrag $ ""

domain :: MonadFail m => Url -> m String
domain (Url u) = case parse pDomain "" u of
                     Left l -> fail (show l)
                     Right r -> return r
    where
    pDomain = (try (string "http://") <|> string "https://") *> many1 (noneOf "/")
