-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Parser where

import Pieces
import Board
import Data.Array
import Data.List

class (Show a) => Parse a where
   parse :: String -> a
    
instance Parse Situation where
    parse str = Situation (makeBoard str) $ readPlayer str

positions :: [Pos]
positions = [Pos c r | r <- enumFromTo One Eight, c <- enumFromTo A H]

makeBoard :: String -> Board
makeBoard str = Board (array (Pos A One, Pos H Eight) $ makeField str)

-- Combines the playfield with their respective positions   
makeField :: String -> [(Pos, Piece)]          
makeField str = zip positions $ readAllTiles str

-- Reads the whole play field
readField :: [String] -> [Piece]
readField [] = []
readField (x:y) = readLine x ++ readField y

-- Reads a whole line and puts the pieces on their respective position in a list
readLine :: String -> [Piece]
readLine str = map (readTile str) [4,6..18]
               
-- Reads from a tile which piece stands on it
readTile :: String -> Int -> Piece
readTile str n = charToPiece $ str!!n

-- Drops the unnecessary string and thereafters reads the play field     
readAllTiles :: String -> [Piece]  
readAllTiles str = readField $ take 8 $ drop 4 $ lines str

-- Determines the player by reading a string
readPlayer :: String -> Player
readPlayer str = choosePlayer $ drop 13 $ head $ lines str

choosePlayer :: String -> Player
choosePlayer x | x == "white" = White
               | otherwise    = Black