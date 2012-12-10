{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import qualified Web.Yahoo.MAService as MA

main :: IO ()
main = do
    resultSet <- MA.parse params "今日も良い天気ですね．"
    case resultSet >>= MA.maResult >>= MA.wordList of
        Just wlist -> forM_ (mapMaybe MA.surface wlist) $ putStrLn . unpack
        Nothing    -> return ()
    where
        params = MA.Params { MA.appId   = "(your Yahoo! App ID)"
                           , MA.results = "ma"
                           }
