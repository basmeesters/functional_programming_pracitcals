-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633


----------------------------------------------------------------------------------------------------------------------------------------------
-- Pleas look at our Lookahead file which is not used for our implementation but is partly finished and could be positive for our final remark
----------------------------------------------------------------------------------------------------------------------------------------------

module Main where

import Pieces
import Board
import Parser
import Rules
--import System -- Does not work in new environments! If so, use import System.Environment instead!
import System.Environment
import Data.Char
import Data.Array

-- The main function, which starts everything
main :: IO()
main = do
    args <- getArgs -- Reading the filename, given as argument
    file <- readFile $ head args
    let start = parse file
    putStrLn "Welcome to our super awesome chess game."
    gameLoop start

-- This function defines a turn
gameLoop :: Situation -> IO()
gameLoop sit@(Situation board@(Board b) player) = do
    putStrLn $ show sit
    -- The next if statement checks if the player has lost
    if hasLost chessRules $ sit then
        putStrLn $ show (opponent player) ++ " has won!"
    else do
        putStrLn $ show player ++ " has to enter his move: (eg. a1a2)"
        regel <- getLine
        let move@(start, end) = (stringToPos regel, stringToPos $ drop 2 regel)
            piece@(Piece p w) = b!start
            validmoves = movesFrom chessRules board start piece
            newsit = Situation (performMove chessRules board move) $ opponent player
        -- The next if statement checks if the player has given input
        if regel /= [] then
            -- The next if statement checks if the input is valid
            if length regel >= 4 && (elem (regel!!0) ['a'..'h']) && (elem (regel!!1) ['1'..'8']) && (elem (regel!!2) ['a'..'h']) && (elem (regel!!3) ['1'..'8']) then
                -- The next if statement checks if the starting position contains a piece from the player
                if p == player then
                    --The next if statement checks if the move is valid
                    if elem end validmoves then
                        gameLoop newsit
                    else do
                        putStrLn "This is not a valid move"
                        putStrLn $ "Here is a list of possible locations to move to from " ++ show (fst move)
                        putStrLn $ show validmoves
                        gameLoop sit
                else do
                    putStrLn $ "This starting position (" ++ show (fst move) ++ ") does not contain a piece you own"
                    gameLoop sit
            else do
                putStrLn "This is not valid input"
                gameLoop sit
        else do
            putStrLn "There is no input"
            gameLoop sit