-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Main where

import Types
import Parser
import Mechanics
import Render
--import System {- Does not work in new environments! If so, use import System.Environment instead! -}
import System.Environment 

main :: IO()
main = do
    args <- getArgs
    file <- readFile $ head args
    let track = readTrack file
    putStrLn "Welcome to our super awesome racetrack game."
    gameLoop track [getStart track] (getStart track, (0,0))
        where getStart (x, _, _) = x

gameLoop :: Track -> Trace -> CarState -> IO()
gameLoop track trace car = do
    raceToSvg "game.svg" track [trace]
    putStrLn "Give input!! (1, 2, 3, 4, 5, 6, 7, 8, 9)"
    regel <- getLine
    let input = head regel
        d = accelerate input
        velocity = ((fst $ snd car) + fst d, (snd $ snd car) + snd d)
        newpos = ((fst $ fst car) + (fst velocity), (snd $ fst car) + (snd velocity))
        newcar = (newpos, velocity)
        newtrace = (fst $ newcar):trace
    if regel /= [] && moveValid car newpos && d /= (6, 6) then
        if not $ crashes car newpos track then
            if not $ completes car newpos track then
                gameLoop track newtrace newcar
            else do
                putStrLn ("You won the game in " ++ (show (length trace + 1)) ++ " steps!")
                einde track newtrace
        else do
            putStrLn "This move crashed the car"
            einde track newtrace
    else do
        putStrLn "This is not a accepted move"
        gameLoop track trace car

einde :: Track -> Trace -> IO()
einde track newtrace = do
    raceToSvg "game.svg" track [newtrace]
    putStrLn "Play again? (Y/N)"
    regel2 <- getLine
    let j = head regel2
    if j == 'y' || j == 'Y' then main else return()

accelerate :: Char -> (Float, Float)
accelerate '1' = (-1, 1)
accelerate '2' = (0, 1)
accelerate '3' = (1, 1)
accelerate '4' = (-1, 0)
accelerate '5' = (0, 0)
accelerate '6' = (1, 0)
accelerate '7' = (-1, -1)
accelerate '8' = (0, -1)
accelerate '9' = (1, -1)
accelerate _ = (6, 6)