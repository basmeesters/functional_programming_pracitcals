-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Rules where

import Pieces
import Board
import Data.Array
import Parser

data Rules = Rules { hasLost :: Situation -> Bool,
                     movesFrom :: Board -> Pos -> Piece -> [Pos],
                     performMove :: Board -> Move -> Board }

-- The set of rules used by chess
chessRules :: Rules
chessRules = Rules chessHasLost chessMovesFrom chessPerformMove

-- This function checks if the player has lost
chessHasLost :: Situation -> Bool
chessHasLost (Situation (Board b) p) = not $ elem (Piece p King) $ elems b
--chessHasLost (Situation board@(Board b) p) = chessMovesFrom board 

-- This function returns a list of possible moves from a certain location when using a certain piece
chessMovesFrom :: Board -> Pos -> Piece -> [Pos]
chessMovesFrom board (Pos c r) (Piece p King)     = filter (checkNotPlayer board p) $ if c < H              then [Pos (succ c) r]        else [] ++
                                                                                      if c > A              then [Pos (pred c) r]        else [] ++
                                                                                      if r < Eight          then [Pos c $ succ r]        else [] ++
                                                                                      if r > One            then [Pos c $ pred r]        else [] ++
                                                                                      if r < Eight && c < H then [Pos (succ c) $ succ r] else [] ++
                                                                                      if r > One   && c < H then [Pos (succ c) $ pred r] else [] ++
                                                                                      if r < Eight && c > A then [Pos (pred c) $ succ r] else [] ++
                                                                                      if r > One   && c > A then [Pos (pred c) $ pred r] else []

chessMovesFrom board pos (Piece p Queen)          = chessMovesFrom board pos (Piece p Rook) ++ chessMovesFrom board pos (Piece p Bishop)

chessMovesFrom board (Pos c r) (Piece p Knight)   = filter (checkNotPlayer board p) $ (if c > A     then (if pred c > A && r      > One   then [Pos (pred $ pred c) $ pred r] else []) ++
                                                                                                         (if pred c > A && r      < Eight then [Pos (pred $ pred c) $ succ r] else [])
                                                                                      else []) ++
                                                                                      (if c < H     then (if succ c < H && r      > One   then [Pos (succ $ succ c) $ pred r] else []) ++
                                                                                                         (if succ c < H && r      < Eight then [Pos (succ $ succ c) $ succ r] else [])
                                                                                      else []) ++
                                                                                      (if r > One   then (if c      > A && pred r > One   then [Pos (pred c) $ pred $ pred r] else []) ++
                                                                                                         (if c      < H && pred r > One   then [Pos (succ c) $ pred $ pred r] else [])
                                                                                      else []) ++
                                                                                      (if r < Eight then (if c      > A && succ r < Eight then [Pos (pred c) $ succ $ succ r] else []) ++
                                                                                                         (if c      < H && succ r < Eight then [Pos (succ c) $ succ $ succ r] else [])
                                                                                      else [])

chessMovesFrom board pos (Piece p Rook)           = checkLeft board pos p     ++ checkRight board pos p     ++ checkDown board pos p   ++ checkUp board pos p

chessMovesFrom board pos (Piece p Bishop)         = checkDownLeft board pos p ++ checkDownRight board pos p ++ checkUpLeft board pos p ++ checkUpRight board pos p

chessMovesFrom board (Pos c r) (Piece White Pawn) = if r < Eight then filter (checkEmpty board) (if r == Two   then [Pos c $ succ $ succ r] ++ [Pos c $ succ r] else [Pos c $ succ r]) ++
                                                                      if c < H then if checkOpponent board White $ Pos (succ c) $ succ r then [Pos (succ c) $ succ r] else [] else [] ++
                                                                      if c > A then if checkOpponent board White $ Pos (pred c) $ succ r then [Pos (pred c) $ succ r] else [] else []
                                                    else []

chessMovesFrom board (Pos c r) (Piece Black Pawn) = if r > One   then filter (checkEmpty board) (if r == Seven then [Pos c $ pred $ pred r] ++ [Pos c $ pred r] else [Pos c $ pred r]) ++
                                                                      if c < H then if checkOpponent board Black $ Pos (succ c) $ pred r then [Pos (succ c) $ pred r] else [] else [] ++
                                                                      if c > A then if checkOpponent board Black $ Pos (pred c) $ pred r then [Pos (pred c) $ pred r] else [] else []
                                                    else []

checkDownRight :: Board -> Pos -> Player -> [Pos]
checkDownRight board pos@(Pos c r) p = if r > One   && c < H && checkNotPlayer board p next then if checkEmpty board next then next:checkDownRight board next p else [next] else []
                                       where next = Pos (succ c) $ pred r

checkDownLeft  :: Board -> Pos -> Player -> [Pos]
checkDownLeft  board pos@(Pos c r) p = if r > One   && c > A && checkNotPlayer board p next then if checkEmpty board next then next:checkDownLeft board next p  else [next] else []
                                       where next = Pos (pred c) $ pred r

checkUpRight   :: Board -> Pos -> Player -> [Pos]
checkUpRight   board pos@(Pos c r) p = if r < Eight && c < H && checkNotPlayer board p next then if checkEmpty board next then next:checkUpRight board next p   else [next] else []
                                       where next = Pos (succ c) $ succ r

checkUpLeft    :: Board -> Pos -> Player -> [Pos]
checkUpLeft    board pos@(Pos c r) p = if r < Eight && c > A && checkNotPlayer board p next then if checkEmpty board next then next:checkUpRight board next p   else [next] else []
                                       where next = Pos (pred c) $ succ r

checkDown      :: Board -> Pos -> Player -> [Pos]
checkDown      board pos@(Pos c r) p = if r > One            && checkNotPlayer board p next then if checkEmpty board next then next:checkDown board next p      else [next] else []
                                       where next = Pos c $ pred r

checkUp        :: Board -> Pos -> Player -> [Pos]
checkUp        board pos@(Pos c r) p = if r < Eight          && checkNotPlayer board p next then if checkEmpty board next then next:checkUp board next p        else [next] else []
                                       where next = Pos c $ succ r

checkLeft      :: Board -> Pos -> Player -> [Pos]
checkLeft      board pos@(Pos c r) p = if c > A              && checkNotPlayer board p next then if checkEmpty board next then next:checkLeft board next p      else [next] else []
                                       where next = Pos (pred c) r

checkRight     :: Board -> Pos -> Player -> [Pos]
checkRight     board pos@(Pos c r) p = if c < H              && checkNotPlayer board p next then if checkEmpty board next then next:checkRight board next p     else [next] else []
                                       where next = Pos (succ c) r

checkEmpty :: Board -> Pos -> Bool
checkEmpty (Board b) pos = b!pos == Piece P N

checkNotPlayer :: Board -> Player -> Pos -> Bool
checkNotPlayer board player pos = getPlayer board pos /= player

checkOpponent  :: Board -> Player -> Pos -> Bool
checkOpponent  board player pos = getPlayer board pos == opponent player

getPlayer :: Board -> Pos -> Player
getPlayer (Board b) pos = (\(Piece p _) -> p) $ b!pos

-- This function moves the piece from position start to position end
chessPerformMove :: Board -> Move -> Board
chessPerformMove (Board b) (start, end) = Board $ b//[(end, b!start), (start, Piece P N)]