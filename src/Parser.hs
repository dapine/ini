module Parser
    ( parseKey
    , parseSectionName
    , parseInteger
    , parseDouble
    , parseBoolean
    ) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Numeric

data INIParser = INIKey String
               | INIString String
               | INIInteger Integer
               | INIDouble Double
               | INIBool Bool 
               | INISectionName String
               | INISection (INIParser, [(String, INIParser)]) deriving (Eq, Show)

parseKey :: Parser INIParser
parseKey = do
    k <- many1 alphaNum
    return $ INIKey k

parseSectionName :: Parser INIParser
parseSectionName = do
    char '['
    sn <- many1 alphaNum
    char ']'
    return $ INISectionName sn

-- FIXME: not supporting negative numbers
parseInteger :: Parser INIParser
parseInteger = liftM (INIInteger . read) $ many1 digit

-- FIXME: not supporting negative numbers
parseDouble :: Parser INIParser
parseDouble = do
    dec <- many1 digit
    char '.'
    fl <- many1 digit
    return $ INIDouble $ fst . head . readFloat $ dec ++ "." ++ fl

parseBoolean :: Parser INIParser
parseBoolean = do
    x <- (string "true" <|> string "false")
    return $ case x of
               "true" -> INIBool True
               "false" -> INIBool False
