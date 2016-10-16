-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Graph where

import Data.Char
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)
import Data.List
import Control.Applicative
import Parser

-- example test:
check = makeIntMap (readTotal p1 [])
  
makeMap :: [Country] -> Int -> [(Int, Country)]
makeMap [] n = []
makeMap (x:xs) n = (n, x): (makeMap xs (n+1))

-- make a IntMap
makeIntMap :: [Country] -> IntMap.IntMap Country
makeIntMap countries = IntMap.fromList (makeMap countries 0)

makeLinks :: Country -> (String, [String])
makeLinks (Country a b) = (a, b)

makeAllLinks :: [Country] -> [(String, [String])]
makeAllLinks countries = map makeLinks countries

-- Maakt een graaf maar het is niet eindig dus nog niet erg nuttig..
--------------------------------------------------------------------------

data Node a = Node
    { label    :: a
    , adjacent :: [Node a]
    }

instance Eq a => Eq (Node a) where
    (Node x _) == (Node y _) = x == y
    
instance (Show a) => Show (Node a) where
    show (Node a b) = show a ++ show b

data Graph a = Graph [Node a]

instance (Show a) => Show (Graph a) where
    show (Graph a) = show a
    
mkGraph :: Eq a => [(a, [a])] -> Graph a
mkGraph links = Graph $ map snd nodeLookupList 
            where mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
                  nodeLookupList = map mkNode links
                  lookupNode lbl = fromJust $ lookup lbl nodeLookupList

-- Een paar standaardfuncties voor een graaf
--------------------------------------------------------------------------

getNode :: Eq a => Graph a -> a -> Node a
getNode (Graph g) x = head $ filter (/= Node x []) g

graphContains :: Eq a => Graph a -> a -> Bool
graphContains (Graph g) x = length (filter (/= Node x []) g) /= 0

removeNodes :: Eq a => Graph a -> [Node a] -> Graph a
removeNodes g [] = g
removeNodes (Graph g) (x:xs) = removeNodes g' xs
                    where
                        g' = Graph $ filter (/= x) g''
                        g'' = map (\(Node a b) -> (Node a $ filter (/= x) b)) g

-- Functies over een graaf
--------------------------------------------------------------------------

shortestPath :: Eq a => Graph a -> a -> a -> [a]
shortestPath graph x y = if graphContains graph x || graphContains graph y then [] else
                            shortestPath' graph [[getNode graph x]] y

shortestPath' :: Eq a => Graph a -> [[Node a]] -> a -> [a]
shortestPath' g paths y = if length (check paths) == 1 then
                            map (\(Node x _) -> x) $ concat $ check paths
                          else
                            shortestPath' (removeNodes g $ map last paths) newpaths y
                        where
                            check x = filter (\x -> last x == Node y []) paths
                            newpaths = concatMap (\x -> (++) <$> [x] <*> [(\(Node _ y) -> y) $ last x]) paths

--minimumSpan graph@(Graph g) x y = 