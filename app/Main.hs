{-# LANGUAGE OverloadedStrings #-}

module Main where

import Validate (validateLine)
import Web.Scotty

main :: IO ()
main = scotty 3003 $ do
  get "/" $ Web.Scotty.text "hello world"

  post "/test" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    line <- jsonData :: ActionM [String]
    json $ map validateLine line
