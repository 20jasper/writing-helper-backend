{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (ToJSON)
import Data.Char (isLower)
import GHC.Generics
import Web.Scotty

main :: IO ()
main = scotty 3003 $ do
  get "/" $ Web.Scotty.text "hello world"

  post "/test" $ do
    addHeader "Access-Control-Allow-Origin" "*"
    line <- jsonData :: ActionM [String]
    json $ map validateLine line

validateLine :: String -> Line
validateLine = validateCapital

uncapitalized = "Sentences should begin with a capital letter"

validateCapital :: [Char] -> Line
validateCapital all@(x : _)
  | isLower x =
      let len = length (takeWhile (/= ' ') all)
       in Line all [Span 0 len (Just uncapitalized), Span len 999999 Nothing]
  | otherwise = Line all [Span 0 9999 Nothing]
validateCapital [] = Line "" [Span 0 0 Nothing]

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

type Error = Maybe String

data Span = Span
  { start :: Int,
    end :: Int,
    error :: Error
  }
  deriving (Generic, Show)

instance ToJSON Span
