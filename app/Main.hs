{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (get, scotty, text)

main :: IO ()
main = scotty 3001 $ do
  get "/" $ text "hello world"
