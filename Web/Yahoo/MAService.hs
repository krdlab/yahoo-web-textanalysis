{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
            , RankNTypes
            #-}
{-# OPTIONS_GHC -Wall #-}
module Web.Yahoo.MAService
    ( Params(..)
    , ResultSet(..)
    , Result(..)
    , MAResult
    , UniqResult
    , Word(..)
    , parse
    , parseResultSet    -- XXX
    ) where

import           Data.Monoid            (mappend)
import           Control.Applicative    ((<$>))
import qualified Data.Conduit           as C
import qualified Network.HTTP.Conduit   as HC
import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  ()
import           Data.Text              (Text, unpack)
import qualified Data.Text.Encoding     as TE
import qualified Data.XML.Types         as XT
import           Text.XML.Stream.Parse

data Params = Params
    { appId :: ByteString
    , results :: ByteString
    }
    deriving Show

data ResultSet = ResultSet
   { maResult :: Maybe MAResult
   , uniqResult :: Maybe UniqResult
   }
   deriving Show

data Result = Result
    { totalCount :: Maybe Int
    , filteredCount :: Maybe Int
    , wordList :: Maybe [Word]
    }
    deriving Show

data Word = Word
    { surface :: Maybe Text
    , reading :: Maybe Text
    , pos     :: Maybe Text
    , baseform :: Maybe Text
    , feature :: Maybe Text
    , count :: Maybe Int
    }
    deriving Show

type MAResult   = Result
type UniqResult = Result

--parse :: (MonadIO m) => Params -> String -> m ResultSet
parse :: Params -> Text -> IO (Maybe ResultSet)
parse ps sentence = do
    -- Failure HC.HttpException m
    request'' <- HC.parseUrl "http://jlp.yahooapis.jp/MAService/V1/parse"
    let request' = request'' { HC.queryString = "appid=" `mappend` appId ps }
    let request = HC.urlEncodedBody [ ("results", results ps)
                                    , ("sentence", TE.encodeUtf8 sentence)
                                    ] request'
    HC.withManager $ \manager -> do
        --HC.Response _ _ _ src <- HC.http request manager
        --src C.$$+- parseBytes def C.$$ parseResultSet
        HC.Response _ _ _ src <- HC.httpLbs request manager
        parseLBS def src C.$$ parseResultSet

parseResultSet :: forall o u. C.Pipe XT.Event XT.Event o u (C.ResourceT IO) (Maybe ResultSet)
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

