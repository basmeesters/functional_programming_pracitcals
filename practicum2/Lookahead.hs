-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633
-- This module is not implemented in our final product but has some functions which could be useful for the sixth exercise.
-- It does compile and has the possibility to show all possible situations for a player during his turn. 
-- Though, through a lack of time we were not able to finish it.

module Lookahead

where 

import Data.MemoTrie
import Pieces
import Board
import Parser
import Rules
import Data.Array
import Data.List

data MoveTree = MoveTree Situation [Situation]

-- Show the current situation and possible situations for the player after his turn
otherSituations :: Situation -> (Situation, [Situation])
otherSituations situation@(Situation board@(Board b) player) = (situation, (makeSituations board player (allMoves a (length a -1))))
                                                                  where a = (apm (Situation board player))
                                                                  
-- Function which makes moves out of the old position and  all possible new positions for one piece
moves :: [(Pos, [Pos])]-> Int -> Int -> [Move]
moves a (-1) k = []
moves a 0 k = [(fst (a!!k), snd (a!!k)!!0)]
moves a n k = (fst (a!!k), snd (a!!k)!!n): (moves a (n-1) k)

-- Makes all the moves for all the pieces
allMoves :: [(Pos, [Pos])] -> Int -> [[Move]]
allMoves a 0= [moves a (length (snd (a!!0)) -1) 0]
allMoves a k= [(moves a (length (snd (a!!k)) -1) k)] ++ allMoves a (k - 1)

-- Show all positions at which the player has pieces and the positions that they can move to
allPossibleMoves :: Situation -> [(Piece, Pos)] -> [(Pos, [Pos])]
allPossibleMoves situation@(Situation board@(Board b) player) [] = []
allPossibleMoves situation@(Situation board@(Board b) player) (x:xs)= (snd x,(chessMovesFrom board (snd x) (fst x))): allPossibleMoves situation xs

-- initiates the allPossibleMoves from the first tile, a1        
apm :: Situation -> [(Pos, [Pos])]                                                                  
apm situation@(Situation board@(Board b) player) = allPossibleMoves situation (allPieces board (stringToPos "a1") player)

-- Makes all possible situations for the player after his turn
makeSituations :: Board -> Player -> [[Move]] -> [Situation]
makeSituations board@(Board b) player [] = []
makeSituations board@(Board b) player (x:xs)= if x /= [] then ((Situation (chessPerformMove board (head x)) (opponent player)):makeSituations board player ((drop 1 x):xs)) 
                                              else makeSituations board player xs                                          

-- Puts all pieces and their positions of one player
allPieces :: Board -> Pos -> Player -> [(Piece, Pos)]
allPieces board@(Board b) pos@(Pos c r) player = if r < Eight 
                                                    then 
                                                    (if c < H 
                                                        then a 
                                                     else (if ((\(Piece p _) -> p) (b!pos)) == player 
                                                                then (b!pos, pos):allPieces board (Pos A (succ r)) player 
                                                           else allPieces board (Pos A (succ r)) player))
                                                 else 
                                                    (if c < H 
                                                        then a
                                                     else (if ((\(Piece p _) -> p) (b!pos)) == player 
                                                                then [(b!pos, pos)] 
                                                           else []))
                                                 where a = (if ((\(Piece p _) -> p) (b!pos)) == player 
                                                                        then (b!pos, pos): (allPieces board (Pos (succ c) r) player) 
                                                                    else allPieces board (Pos (succ c) r) player) 


