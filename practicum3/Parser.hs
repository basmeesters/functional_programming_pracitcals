-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Parser where

import TooSimpleParseLib
import Data.Char
import Data.List
-- import Text.ParserCombinators.Parsec

type Border = (String, String)
data Country = Country String [String]

-- Check if countries are the same (independently of their borders)
instance Eq Country where
    (Country x _) == (Country y _) = x == y

instance Show Country where
    show (Country a b) = show a ++ "=" ++ show b

-- Reads borders in, in both ways
readTotal :: [(String, String)] -> [Country] -> [Country]
readTotal (x:xs) list = readCountries (x:xs) (readCountries (x:xs) list fst snd) snd fst

-- Adds country with Country op and border op' to a list of countries
readCountries :: [(String, String)] -> [Country] -> ((String, String) -> String) -> ((String, String) -> String) -> [Country]
readCountries [] list op op' = list
readCountries (x:xs) list op op'= readCountries xs (checkCountry (getCountry op op' x) list list) op op'

-- Check if a country is unique
checkCountry :: Country -> [Country] -> [Country] -> [Country]
checkCountry c [] list = c:list
checkCountry c (x:xs) list = if c == x then (addBorders x c):(delete c list) else checkCountry c xs list

-- Add borders
addBorders :: Country -> Country -> Country
addBorders (Country c borders) (Country _ borders') = Country c (borders ++ borders')

-- Makes a Country out of a tuple
getCountry :: ((String, String) -> String) -> ((String, String) -> String) -> (String, String) -> Country
getCountry op op' c = Country (op c) [op' c]

-- Creates a tuple of two countries from the input
pBorder :: Parser Char (String, String)
pBorder = (\a b -> (a,b)) <$> (pMany pRandomWord *> pCountry <|> pCountry) <* pMany pRandomWord <* pMany pRandomWord <*> pCountry

-- Simple Parse functions from lectures
pMany p = (:) <$> p <*> pMany p `option` []
pMany1 p = (:) <$> p <*> pMany p
p `option` v = p <|> pSucceed v

-- Parse functions to filter Countries from a sentence
pCountry = (:) <$> pSatisfy isUpper <*> pMany (pSatisfy isAlpha) 
pRandomWord =  pToken " " <|> pRandom
pRandom = pMany1 (pSatisfy isLower)

-- example test
test = readTotal p1 []

-- Example
p1 = map (doParser pBorder) (lines "Nederland is north from Belgium.\n Germany is east from Nederland. \n Nederland is France. \n Belgium is France. \n Germany is next to France.")
doParser p s = snd $ head $ runParser p s