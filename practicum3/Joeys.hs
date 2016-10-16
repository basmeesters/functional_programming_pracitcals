--import Types
import Data.Char
import qualified Data.IntMap as IntMap
import Data.List
import TooSimpleParseLib
--import Graph

main = do   file <- readFile "landen.txt"
            let regels = lines file
            let parsed = map (doParser pBorder2) regels
            let lands   = readLands parsed []
            --putStrLn ("lands: " ++ show lands)
            let ref     = getReference $ nub $ map fst' parsed
            --putStrLn ("ref: " ++ show ref)
            let graph = landsToGraph lands
            --putStrLn ("new: " ++ show graph)            
            a <- getLine
            b <- getLine
            let path    = getMinPath graph a b
            let path'   = hashBack ref path
            print path'
            --print ref
            
            --print lands
          
doParser p s = snd $ head $ runParser p s

-- borderlands (>^^)>
type BorderLand = (String, String, String)

-- Builds a list of lands (with their relations) from the parse results.
readLands :: [BorderLand] -> [Land] -> [Land]
readLands []     acc   = acc
readLands (x:xs) acc = readLands xs (insertLand acc (getLand x)) 

-------------------
-- PARSER STUFFZ --
-------------------

-- pBorder1 geeft het resultaat met als extra constraint dat de opvullende text tussen de keywords vast staan (je voorkomt gibberish)
-- pBorder2 geeft nu het resultaat met de volgende constraints op de input:
        -- de keywords Land, Richting en Land moeten in deze volgorde in de regel staan.
        -- enkel en alleen de keywords mogen met een hoofdletter beginnen
        -- de regel moet eindigen met een .
pBorder1 = (\a r b -> (a,r,b)) <$> pCountry <* pMany pSomeWord <*> pRichting <* pMany pSomeWord <*> pCountry <* pToken "."
pBorder2 = (\a r b -> (a,r,b)) <$> (pMany pRandomWord *> pCountry <|> pCountry) <* pMany pRandomWord <*> pRichting <* pMany pRandomWord <*> pCountry <* pToken "."


-- Main parsers (naar DataType)          
pCountry :: Parser Char [Char]
pCountry = (:) <$> pSatisfy isUpper <*> pMany (pSatisfy isAlpha)     

-- Parses pre-defined words that ressemble a strict language 
pSomeWord = pToken "lies" <|> pToken "to" <|> pToken "the" <|> pToken "is" <|>
            pToken "of" <|> pToken " "

-- Parses any random word that has only lowercase letters.
pRandomWord =  pToken " " <|> pRandom
pRandom = pMany1 (pSatisfy isLower)

-- Parses a direction.
pRichting = pToken "North" <|> pToken "South" <|> pToken "East" <|> pToken "West" 
                              

-------------------
-- OTHER STUFFZ ---
-------------------                             
-- Hardcodes debugging tests
p1 = doParser pBorder2 "Nederland is fucking North from motherfucking gay ass pussy Belgium."
p2 = doParser pBorder2 "France is fucking South from motherfucking gay ass pussy Belgium."
p3 = doParser pBorder2 "Germany is fucking East from motherfucking gay ass pussy Belgium."
p4 = doParser pBorder2 "Germany is fucking East from motherfucking gay ass pussy Holland."
l = insertLand [] (getLand p1)
l' = insertLand l (getLand p2)
l''= insertLand l' (getLand p3)
l''' = insertLand l'' (getLand p4)


--landsToGraph :: [Land] -> Graph
landsToGraph landen = IntMap.fromList (map (\l -> (hashString 0 (getC' l), bordersToList (getBorders l))) landen)

bordersToList borders = map (\b -> hashString 0 (getC b)) borders

-- Adds a land into the list of Lands. Updates the lands if they're not unique
insertLand :: [Land] -> Land -> [Land]
insertLand landen l = case land of 
                                Just l'  -> updateLanden landen l l' []
                                Nothing -> l : landen
                            where land = searchLand landen $ getCountry l

 -- Removes old Land and adds new Land from the list             
updateLanden :: [Land] -> Land -> Land -> [Land] -> [Land]
updateLanden []     old new accum = error "Land not found. Check your input!"
updateLanden (x:xs) old new accum
    | (getCountry x) == (getCountry new) = updateLand old new : accum ++ xs
    | (getCountry x) /= (getCountry new) = updateLanden xs old new (x : accum)                                                           

-- Checks if a land already excists in the list of lands (checks for uniqueness)
searchLand :: [Land] -> Country -> Maybe Land
searchLand [] c      = Nothing
searchLand (x:xs) c 
        | country == c = Just x
        | country /= c = searchLand xs c
             where country = getCountry x

-- If a land is not unique add its borders to the current land.             
updateLand :: Land -> Land -> Land
updateLand (Land (c, borders)) (Land (_, borders')) = 
                        Land (c, borders' ++ borders)

-------------------------------------------------------------------------------
-- Returns a Land from a 3tuple of strings (parse result)
getLand :: BorderLand -> Land
getLand t = Land (Country (thrd' t), [getBorder (fst' t, snd' t)])

-- Returns a Border from a 2tuple of strings (parse result)       
getBorder :: (String, String) -> Border
getBorder tuple = Border (Country (fst tuple), Direction (snd tuple))

-- These functions return a specific datastructure from a Land type
getCountry :: Land -> Country
getCountry (Land (c, _)) = c
getBorders :: Land -> [Border]
getBorders (Land (_, b)) = b



-- 3Tuple functions
fst' (x,y,z) = x
snd' (x,y,z) = y
thrd' (x,y,z)= z 