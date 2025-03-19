module MyLib (gluePercentage) where

import Data.Char (toLower)
import Data.List (genericLength)

gluePercentage :: String -> Double
gluePercentage "" = 0
gluePercentage xs =
  let lower = words $ map toLower xs
      total = genericLength lower
      glue = genericLength $ filter (`elem` glueWords) lower
   in (glue / total) * 100

-- https://debbiesantosgoncalves.naiwe.com/files/2023/05/Glue-Words-and-Sticky-Sentences.pdf
glueWords :: [String]
glueWords = ["a", "about", "an", "and", "any", "asked", "be", "but", "by", "does", "down", "even", "every", "for", "from", "get", "go", "have", "herself", "himself", "if", "in", "into", "is", "it", "just", "like", "literally", "main", "make", "much", "new", "of", "on", "really", "said", "should", "simply", "so", "some", "such", "than", "that", "the", "then", "there", "therefore", "think", "this", "to", "up", "until", "very", "was", "well", "what", "when", "will", "with"]

gluePhrases = ["in order to", "in regards to", "over into", "start to", "begin to", "getting to", "as though", "to there", "such as", "very much"]
