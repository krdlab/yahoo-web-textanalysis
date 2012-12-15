{-# LANGUAGE OverloadedStrings #-}
module Web.Yahoo.MAService.Types
    ( Params(..)
    , defaultRequestParams
    , PartOfSpeech(..)
    , AnalysisResultType(..)
    , ResponseElement(..)
    , ResultSet(..)
    , Result(..)
    , MAResult
    , UniqResult
    , Word(..)
    )
    where

import           Data.ByteString              (ByteString)
import           Data.ByteString.Char8        ()
import           Data.Text                    (Text)

-- | リクエストパラメータ
data Params = Params
    { paramAppId            :: ByteString
    , paramResults          :: [AnalysisResultType]
    , paramResponse         :: Maybe [ResponseElement]
    , paramFilter           :: Maybe [PartOfSpeech]
    , paramMaResponse       :: Maybe [ResponseElement]
    , paramMaFilter         :: Maybe [PartOfSpeech]
    , paramUniqResponse     :: Maybe [ResponseElement]
    , paramUniqFilter       :: Maybe [PartOfSpeech]
    , paramUniqByBaseform   :: Maybe Bool
    }
    deriving (Show, Eq)

-- | 品詞
data PartOfSpeech = Adjective           -- ^ 形容詞
                  | AdjectiveVerb       -- ^ 形容動詞
                  | Interjection        -- ^ 感動詞
                  | Adverb              -- ^ 副詞
                  | PreNounAdjectival   -- ^ 連体詞
                  | Conjunction         -- ^ 接続詞
                  | Prefix              -- ^ 接頭辞
                  | Suffix              -- ^ 接尾辞
                  | Noun                -- ^ 名詞
                  | Verb                -- ^ 動詞
                  | Particle            -- ^ 助詞
                  | AuxiliaryVerb       -- ^ 助動詞
                  | Special             -- ^ 特殊 (句読点，括弧，記号等)
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data AnalysisResultType = MA | Uniq
                        deriving (Show, Eq, Ord, Read, Enum, Bounded)

data ResponseElement = Surface
                     | Reading
                     | Pos
                     | Baseform
                     | Feature
                     deriving (Show, Eq, Ord, Read, Enum, Bounded)

defaultRequestParams :: Params
defaultRequestParams = Params
    { paramAppId            = ""
    , paramResults          = [MA]
    , paramResponse         = Nothing
    , paramFilter           = Nothing
    , paramMaResponse       = Nothing
    , paramMaFilter         = Nothing
    , paramUniqResponse     = Nothing
    , paramUniqFilter       = Nothing
    , paramUniqByBaseform   = Nothing
    }

data ResultSet = ResultSet
   { maResult :: Maybe MAResult
   , uniqResult :: Maybe UniqResult
   }
   deriving Show

type MAResult   = Result
type UniqResult = Result

data Result = Result
    { totalCount :: Maybe Int
    , filteredCount :: Maybe Int
    , wordList :: Maybe [Word]
    }
    deriving Show

data Word = Word
    { surface  :: Maybe Text
    , reading  :: Maybe Text
    , pos      :: Maybe Text
    , baseform :: Maybe Text
    , feature  :: Maybe Text
    , count    :: Maybe Int
    }
    deriving Show

