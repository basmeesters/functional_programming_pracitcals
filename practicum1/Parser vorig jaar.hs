module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.UU as UU
import Data.Map
import Char
import Control.Monad

--Mogelijke instellingen zijn numPlayers en startChips
--Invoeringen zijn in het bestand configuration.txt in de vorm:
--Instelling=waarde
type Configuratie = Data.Map.Map String String

ident :: Parser String
ident = do x <- letter
           xs <- many (letter UU.<|> digit UU.<|> char '_')
           return (x:xs)
    <?> "identifier"

nieuweRegel :: Parser ()
nieuweRegel = do oneOf "\n\r"
                 return ()
    <?> "end of line"

instelling :: Parser (String, String)
instelling = do naam <- ident
                char '='
                waarde <- manyTill anyChar (try nieuweRegel UU.<|> try eof)
                return (naam, reverse $ dropWhile isSpace $ reverse waarde)

leesRegel :: Parser (String, String)
leesRegel = do skipMany space
               try (instelling >>= return)

leesFile :: Parser [(String, String)]
leesFile = do regels <- many leesRegel
              return regels

leesConfiguratie :: SourceName -> IO (Either ParseError Configuratie)
leesConfiguratie naam = parseFromFile leesFile naam >>= return . fmap (Data.Map.fromList)