module Parser
    ( parseKey
    , parseSectionName
    , parseInteger
    , parseString
    , parseBareString
    , parseDouble
    , parseBoolean
    , parseValue
    , parseKeyValue
    ) where

import Text.ParserCombinators.Parsec

data INIParser = INIKey String
               | INIString String
               | INIInteger Integer
               | INIDouble Double
               | INIBool Bool 
               | INISectionName String
               | INIKeyValue (INIParser, INIParser)
               | INISection (INIParser, [INIParser]) deriving (Eq, Show)

sign :: Parser String
sign = string "-" <|> string "+" <|> string ""

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

-- INCOMPLETE: allow all characters
parseString :: Parser INIParser
parseString = do
    char '"'
    s <- many1 alphaNum
    char '"'
    return $ INIString s

-- INCOMPLETE: allow all characters
parseBareString :: Parser INIParser
parseBareString = do
    s <- many1 alphaNum
    return $ INIString s

parseInteger :: Parser INIParser
parseInteger = do
    s <- sign
    n <- many1 digit
    return $ INIInteger $ case s of
                            "-" -> read ("-" ++ n) :: Integer
                            _ -> read n :: Integer

parseDouble :: Parser INIParser
parseDouble = do
    s <- sign
    dec <- many1 digit
    char '.'
    fl <- many1 digit
    return $ INIDouble $ case s of
                           "-" -> read ("-" ++ dec ++ "." ++ fl) :: Double
                           _ -> read (dec ++ "." ++ fl) :: Double

parseBoolean :: Parser INIParser
parseBoolean = do
    x <- (string "true" <|> string "false")
    return $ case x of
               "true" -> INIBool True
               "false" -> INIBool False

parseValue :: Parser INIParser
parseValue = try parseDouble <|> parseInteger <|> parseBoolean <|> parseString <|> parseBareString

parseKeyValue :: Parser INIParser
parseKeyValue = do
    k <- parseKey
    char '='
    v <- parseValue
    return $ INIKeyValue (k, v)
