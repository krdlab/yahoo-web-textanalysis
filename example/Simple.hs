{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Web.Yahoo.MAService as MA

main :: IO ()
main = do
    resultSet <- MA.parse params "今日は良い天気ですね．"
    print resultSet
    where
        params = MA.defaultRequestParams { MA.paramAppId   = "(your Yahoo! App ID)"
                                         , MA.paramResults = [MA.MA, MA.Uniq]
                                         }
