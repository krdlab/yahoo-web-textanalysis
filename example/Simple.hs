{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Web.Yahoo.MAService as MA

main :: IO ()
main = do
    resultSet <- MA.parse params "今日も良い天気ですね．"
    print resultSet
    where
        params = MA.Params { MA.appId   = "(your Yahoo! App ID)"
                           , MA.results = "ma,uniq"
                           }
