module MainModule where

import AI
import Combo
import Deck
import Cards
import qualified Data.Map as M
import Data.List
--This module contains the actual program that will be run.
--The program is started by the main method.
--It gives a quick intro to the game, asks if the user wants to play
--and starts the game if he wants to.
--main :: IO ()
--main = do putStrLn "Welcome to \"Ultimate poker: Super HD Remix\""
--          putStrLn "Do you want to play a game?"
--          putStrLn "Yes/No"
--          c <- getLine
--          if (not (length c == 2&& (c!!0=='N'||c!!0=='n')&&(c!!1=='O'||c!!1=='o')))
--          then do game
--                  main
--          else return () 

--The game method contains most of the actual program.
--It forces deals 10 cards and divides them over the two players
--Then it asks the player which cards they want to switch,
--let's the computer do the same and then finally display who has won
--and what the final hands were.		  
game :: IO ()
game = do deck <- shuffler fullDeck 10000
          let ([a,b,c,d,e,f,g,h,i,j],y) = dealNCards 10 deck
              playerHand    = sortHand (Hand a b c d e)
              computerHand  = Hand f g h i j
              takenDeck     = y
          showHand playerHand
          putStrLn "Which cards do you want to switch? \n  Maximum of 3!"
          switchCards <- askCardsForExchange
          let (newPlayerHand, playerSwitchedDeck) = exchangeCards playerHand switchCards takenDeck
              compSwitch = pickSubst computerHand
              (newCompHand, finalDeck) = exchangeCards computerHand compSwitch playerSwitchedDeck   
          putStrLn "The computer is thinking."
          putStrLn ("Your new hand is: " ++ (show (sortHand newPlayerHand)))
          putStrLn ("The computer's new hand is: " ++ (show (sortHand newCompHand)))
          putStrLn "The result is:"
          putStrLn ("You have " ++ (show (whoWin newPlayerHand newCompHand)))
          putStrLn "Thanks for playing"
          putStrLn ""
          return ()     
           
--The shuffler method shuffles a given deck i times.
--It uses the method shufflerIO for all shuffles but the first.              
shuffler :: Deck -> Int -> IO Deck
shuffler deck i = shufflerIO (shuffleDeck deck) (i-1)

shufflerIO :: IO Deck -> Int -> IO Deck
shufflerIO deckIO 0 = do c <- deckIO
                         shuffleDeck c
shufflerIO deckIO i = do c <- deckIO
                         shufflerIO (shuffleDeck c) (i-1)

--Showhand is the method that displays a given hand to the console.
showHand :: Hand -> IO ()
showHand hand = do putStrLn (show hand)

--Askcardsforexchange takes in a string from the player and dissects
--it to find out what cards he wants to switch from his hand.
askCardsForExchange :: IO [Card]
askCardsForExchange = do c <- getLine
                         let list = words c
                         if(length list < 4)
                         then return (readCards list)
                         else do putStrLn "Teveel kaarten, kies opnieuw"
                                 askCardsForExchange

readCards :: [String] -> [Card]
readCards string = map readCard string

readCard :: String -> Card
readCard [a,b]      = Card (charToWorth a) (charToSuit b)
readCard [a,b,c]    = Card Ten (charToSuit c)

charToWorth :: Char -> Worth 
charToWorth '2' = Two
charToWorth '3' = Three 
charToWorth '4' = Four 
charToWorth '5' = Five 
charToWorth '6' = Six
charToWorth '7' = Seven
charToWorth '8' = Eight
charToWorth '9' = Nine
charToWorth 'J' = Jack  
charToWorth 'Q' = Queen
charToWorth 'K' = King
charToWorth 'A' = Ace

charToSuit :: Char -> Suit
charToSuit 'H' = Hearts 
charToSuit 'D' = Diamonds
charToSuit 'C' = Clubs 
charToSuit 'S' = Spades

--ExchangeCards takes a hand and a set of carts and switches
--the given cards out of the hand and adds new cards to a total of 5 cards.
exchangeCards :: Hand -> [Card] -> Deck -> (Hand, Deck)
exchangeCards (Hand a b c d e) switch deck = (newHand, newDeck) where
    (newCards, newDeck) = dealNCards (5 - (length ([a,b,c,d,e] \\ switch))) deck
    [v,w,x,y,z] = ([a,b,c,d,e] \\ switch) ++ newCards
    newHand = Hand v w x y z