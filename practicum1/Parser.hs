-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Parser where

import Types

readFloat :: String -> Float
readFloat str = case reads str of
	[] -> error "not a floating point number"
	(p,_):_ -> p

readInt :: String -> Int
readInt str = case reads str of
	[] -> error "not an integer"
	(p,_):_ -> p
	
-- readTrack that uses a string to make a tuple of three elements with starting point first, then the finishline and the boundarylines as last
readTrack :: String -> Track
readTrack str = (makePoint $ convert str, makeLine $ drop 2 $ convert str, makeBoundaries $ drop 6 $ convert str)

-- Convert the string into a list of floats	
convert :: String -> [Float]
convert x = map readFloat $ words x

-- Make a point out of the first two elements of a list of floats
makePoint :: [Float] -> Point
makePoint (x:xs) = (x, head xs)

-- Make a line out of the first four elements of a list of floats
makeLine :: [Float] -> Line
makeLine x = (makePoint x, makePoint $ drop 2 x)

-- Make lines out of the whole list of given floats
makeBoundaries :: [Float] -> [Line]
makeBoundaries x = map makeLine $ splitEvery 4 x

-- Function that splits the list in groups of four
-- Needed because the makeBoundaries expects a list of lists with four elements each
splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n list = x : (splitEvery n y)
  where(x,y) = splitAt n list