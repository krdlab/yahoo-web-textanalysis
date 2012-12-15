{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Web.Yahoo.MAService as MA
import Web.Yahoo.MAService.Internal

-- TODO: test
main :: IO ()
main = do
    print $ pairsOfHeader params
    print $ pairsOfBody params "ほげほげ"
    where
        params = MA.defaultRequestParams { MA.paramAppId    = "(your Yahoo! App ID)"
                                         , MA.paramResults  = [MA.MA, MA.Uniq]
                                         , MA.paramResponse = Just [MA.Surface, MA.Reading]
                                         , MA.paramFilter   = Just [MA.Adjective, MA.Verb]
                                         }
