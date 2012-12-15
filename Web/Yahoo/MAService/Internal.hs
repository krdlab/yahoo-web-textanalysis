{-# LANGUAGE OverloadedStrings #-}
module Web.Yahoo.MAService.Internal
    ( pairsOfHeader
    , pairsOfBody
    )
    where

import Data.Monoid (mappend)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BS
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)
import qualified Data.List as L
import Data.Char (toLower)
import Network.HTTP.Types.Header (HeaderName, hUserAgent)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Web.Yahoo.MAService.Types

pairsOfHeader :: Params -> [(HeaderName, ByteString)]
pairsOfHeader params =
    [(hUserAgent, "Yahoo AppID: " `mappend` paramAppId params)]

pairsOfBody :: Params -> Text -> [(ByteString, ByteString)]
pairsOfBody params sentence =
    [ ("results", intercalateComma $ paramResults params)
    , ("sentence", TE.encodeUtf8 sentence)
    ]
    ++ mapMaybe maybeHeader (zip names values)
    where
        maybeHeader (name, record) = case record params of
                                      Just v  -> Just (name, v)
                                      Nothing -> Nothing
        names  = ["response", "filter", "ma_response", "ma_filter", "uniq_response", "uniq_filter", "uniq_by_baseform"]
        values = [ paramResponse >=> return . intercalateComma
                 , paramFilter >=> return . toFilter
                 , paramMaResponse >=> return . intercalateComma
                 , paramMaFilter >=> return . toFilter
                 , paramUniqResponse >=> return . intercalateComma
                 , paramUniqFilter >=> return . toFilter
                 , paramUniqByBaseform >=> return . BS.pack . show
                 ]
        intercalateComma es = BS.pack $ L.intercalate "," $ map (show >=> return . toLower) es
        toFilter  es = BS.pack $ L.intercalate "|" $ map (show . succ . fromEnum) es
