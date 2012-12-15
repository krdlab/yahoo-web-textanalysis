{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import qualified Web.Yahoo.MAService as MA

main :: IO ()
main = do
    MA.parse params1 "今日は良い天気ですね．" >>= printSurface
    MA.parse params2 "今日は良い天気ですね．" >>= print
    where
        params1 = MA.defaultRequestParams { MA.paramAppId    = "(your Yahoo! App ID)"
                                          , MA.paramResults  = [MA.MA]
                                          }
        params2 = MA.defaultRequestParams { MA.paramAppId    = "(your Yahoo! App ID)"
                                          , MA.paramResults  = [MA.MA]
                                          , MA.paramResponse = Just [MA.Surface]
                                          }
        printSurface rs = case rs >>= MA.maResult >>= MA.wordList of
                                     Just wlist -> forM_ (mapMaybe MA.surface wlist) $ putStrLn . unpack
                                     Nothing    -> return ()
