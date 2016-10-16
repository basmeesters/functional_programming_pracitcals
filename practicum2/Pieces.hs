-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

-- In this module we define all chess pieces and give them a show instance.
module Pieces where

import Data.Char

-- Defining the players
data Player = Black
            | White
            | P
    deriving (Eq)

instance Show Player where
    show Black = "Black"
    show White = "White"

opponent :: Player -> Player
opponent Black = White
opponent White = Black

-- Defining the different pieces
data Worth = King
           | Queen
           | Knight
           | Rook
           | Bishop
           | Pawn
           | N
    deriving (Eq)

instance Show Worth where
    show King   = "K"
    show Queen  = "Q"
    show Knight = "N"
    show Rook   = "R"
    show Bishop = "B"
    show Pawn   = "P"

-- Combining the players and pieces
data Piece = Piece Player Worth
    deriving (Eq)

-- This shows the white pieces as lowercase and black pieces as uppercase.
instance Show Piece where
    show (Piece White w) = map toLower $ show w
    show (Piece Black w) = show w
    show (Piece P N)     = " "

-- This function converts chars to their respective pieces, which is usefull for parsing
charToPiece :: Char -> Piece
charToPiece 'K' = Piece Black King
charToPiece 'k' = Piece White King
charToPiece 'Q' = Piece Black Queen
charToPiece 'q' = Piece White Queen
charToPiece 'N' = Piece Black Knight
charToPiece 'n' = Piece White Knight
charToPiece 'R' = Piece Black Rook
charToPiece 'r' = Piece White Rook
charToPiece 'B' = Piece Black Bishop
charToPiece 'b' = Piece White Bishop
charToPiece 'P' = Piece Black Pawn
charToPiece 'p' = Piece White Pawn
charToPiece _   = Piece P N