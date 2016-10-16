-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Board where

import Pieces
import Data.Array
import Data.List

-- Defining a data type for Columns
data Column = A | B | C | D | E | F | G | H
    deriving (Eq, Ord, Enum, Ix)

instance Show Column where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show H = "h"

-- This function converts chars to their respective column letters
charToColumn :: Char -> Column
charToColumn 'a' = A
charToColumn 'b' = B
charToColumn 'c' = C
charToColumn 'd' = D
charToColumn 'e' = E
charToColumn 'f' = F
charToColumn 'g' = G
charToColumn 'h' = H

-- Defining a data type for rows
data Row = One | Two | Three | Four | Five | Six | Seven | Eight
    deriving (Eq, Ord, Enum, Ix)

instance Show Row where
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"

-- This function converst chars to their respective row numbers
charToRow :: Char -> Row
charToRow '1' = One
charToRow '2' = Two
charToRow '3' = Three
charToRow '4' = Four
charToRow '5' = Five
charToRow '6' = Six
charToRow '7' = Seven
charToRow '8' = Eight

-- Combining the data types Row and Column to make a data type Pos(ition)
data Pos = Pos Column Row
    deriving (Eq, Ord, Ix)

-- This function converts a string consisting of a char and a number to a Position
stringToPos :: String -> Pos
stringToPos (x:y:ys) = Pos (charToColumn x) $ charToRow y

instance Show Pos where
    show (Pos c r) = show c ++ show r

-- A type to hold two positions
type Move = (Pos, Pos)

-- Defining the board
data Board = Board (Array Pos Piece)

instance Show Board where
    show board = "    a b c d e f g h\n  +-----------------+\n1 | " ++ showRow board One ++
                           " | 1\n2 | " ++ showRow board Two   ++
                           " | 2\n3 | " ++ showRow board Three ++
                           " | 3\n4 | " ++ showRow board Four  ++
                           " | 4\n5 | " ++ showRow board Five  ++
                           " | 5\n6 | " ++ showRow board Six   ++
                           " | 6\n7 | " ++ showRow board Seven ++
                           " | 7\n8 | " ++ showRow board Eight ++
                           " | 8\n  +-----------------+\n    a b c d e f g h"

showRow :: Board -> Row -> String
showRow (Board b) r = intersperse ' ' $ concat $ map (\x -> show $ b!(Pos x r)) $ enumFromTo A H

-- Defining the situation type, which holds the current board and the player whose turn is next
data Situation = Situation Board Player

instance Show Situation where
    show (Situation b p) = "Next player: " ++ show p ++ "\n\n" ++ show b