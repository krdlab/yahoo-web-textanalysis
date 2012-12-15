{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , RankNTypes
            #-}
{-# OPTIONS_GHC -Wall #-}
module Web.Yahoo.MAService
    ( Params(..)
    , PartOfSpeech(..)
    , AnalysisResultType(..)
    , ResponseElement(..)
    , defaultRequestParams
    , ResultSet(..)
    , Result(..)
    , MAResult
    , UniqResult
    , Word(..)
    , parse
    , buildRequest
    , parseResultSet
    ) where

import           Control.Applicative          ((<$>))
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO, MonadResource, ResourceT)
import           Control.Failure              (Failure)
import qualified Data.Conduit                 as C
import qualified Network.HTTP.Conduit         as HC
import           Data.Text                    (Text, unpack)
import qualified Data.XML.Types               as XT
import           Text.XML.Stream.Parse

import Web.Yahoo.MAService.Types
import Web.Yahoo.MAService.Internal

endpoint :: String
endpoint = "http://jlp.yahooapis.jp/MAService/V1/parse"

parse :: (Failure HC.HttpException m, MonadIO m, MonadThrow m, MonadUnsafeIO m, MonadBaseControl IO m, MonadResource (ResourceT m))
      => Params
      -> Text
      -> m (Maybe ResultSet)
parse params sentence = do
    request <- buildRequest params sentence
    parse' request

parse' :: (Failure HC.HttpException m, MonadIO m, MonadThrow m, MonadUnsafeIO m, MonadBaseControl IO m, MonadResource (ResourceT m))
       => HC.Request (ResourceT m)
       -> m (Maybe ResultSet)
parse' request = HC.withManager $ \manager -> do
    HC.Response _ _ _ src <- HC.httpLbs request manager
    parseLBS def src C.$$ parseResultSet

buildRequest :: (Monad m, Failure HC.HttpException m, Monad m')
             => Params
             -> Text
             -> m (HC.Request m')
buildRequest params sentence = url endpoint >>= addHeader >>= addBody
    where
        url           = HC.parseUrl
        addHeader req = return $ req { HC.requestHeaders = HC.requestHeaders req ++ pairsOfHeader params }
        addBody   req = return $ HC.urlEncodedBody (pairsOfBody params sentence) req

parseResultSet :: (MonadResource m) => C.Pipe XT.Event XT.Event o u m (Maybe ResultSet)
parseResultSet = tagName (nsName "ResultSet") ignoreAttrs $ \_ -> do
    maResult'   <- parseResult $ nsName "ma_result"
    uniqResult' <- parseResult $ nsName "uniq_result"
    return $ ResultSet maResult' uniqResult'
    where
        nsName ln = XT.Name ln (Just "urn:yahoo:jp:jlp") Nothing

        parseResult tname = tagNoAttr tname $ do
            totalCount'    <- tagNoAttr (nsName "total_count") content
            filteredCount' <- tagNoAttr (nsName "filtered_count") content
            wordList'      <- tagNoAttr (nsName "word_list") $ many parseWord
            return $ Result
                        (read . unpack <$> totalCount')
                        (read . unpack <$> filteredCount')
                        wordList'

        parseWord = tagNoAttr (nsName "word") $ do
            count'    <- tagNoAttr (nsName "count") $ do
                c <- content
                return . read . unpack $ c
            surface'  <- tagNoAttr (nsName "surface") content
            reading'  <- tagNoAttr (nsName "reading") content
            pos'      <- tagNoAttr (nsName "pos") content
            baseform' <- tagNoAttr (nsName "baseform") content
            feature'  <- tagNoAttr (nsName "feature") content
            return $ Word surface' reading' pos' baseform' feature' count'

