{-# LANGUAGE DeriveGeneric #-}

module Validate (validateLine, Line) where

import Data.Aeson (ToJSON)
import Data.Char (isLower)
import GHC.Generics (Generic)
import MyLib (gluePercentage)
import Text.Printf (printf)

-- I want to measure readability per clause and per sentence
-- Y.M.C.A/Ph.D/...
-- syllables vary per dialect and accent
-- hapax legomena

-- let's start more easier with sentence doesn't start with caps

validateLine :: String -> [Line]
validateLine xs = [validateCapital xs, validateGlue xs]

rangeEnd :: Int
rangeEnd = 999999999

validateGlue :: String -> Line
validateGlue xs
  | gluePercentage xs > 60 = line err
  | otherwise = line Nothing
  where
    line x = Line xs [Span 0 rangeEnd x]
    err = Just $ printf "Glue percentage too high: %.1f%%" $ gluePercentage xs

validateCapital :: [Char] -> Line
validateCapital all@(x : _)
  | isLower x =
      let len = length (takeWhile (/= ' ') all)
       in Line all [Span 0 len err, Span len 999999 Nothing]
  | otherwise = Line all [Span 0 9999 Nothing]
  where
    err = Just "Sentences should begin with a capital letter"
validateCapital [] = Line "" [Span 0 0 Nothing]

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
