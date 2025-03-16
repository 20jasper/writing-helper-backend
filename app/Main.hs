{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (ToJSON)
import GHC.Generics
import Web.Scotty

main :: IO ()
main = scotty 3003 $ do
  get "/" $ Web.Scotty.text "hello world"

  get "/test" $
    json
      [ Line
          "hi how's it going"
          [Span 0 1 Ok, Span 1 10 Error, Span 10 99999 Ok]
      ]

-- I want to measure readability per clause and per sentence
-- Y.M.C.A/Ph.D/...
-- syllables vary per dialect and accent
-- hapax legomena

-- let's start more easier with sentence doesn't start with caps

data Line = Line
  { text :: String,
    spans :: [Span]
  }
  deriving (Generic, Show)

instance ToJSON Line

data Kind = Error | Ok
  deriving (Generic, Show)

instance ToJSON Kind

data Span = Span
  { start :: Int,
    end :: Int,
    kind :: Kind
  }
  deriving (Generic, Show)

instance ToJSON Span
