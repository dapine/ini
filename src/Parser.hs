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
    , parseComment
    , parseSectionKeyValues
    , parsePlainConfig
    , parseINI
    ) where

import Text.ParserCombinators.Parsec

data INIParser = INIKey String
               | INIString String
               | INIInteger Integer
               | INIDouble Double
               | INIBool Bool 
               | INIComment String
               | INISectionName String
               | INIKeyValue (INIParser, INIParser)
               | INISection (INIParser, [INIParser]) 
               | INIConfig [INIParser]
               | INI [INIParser] deriving (Eq, Show)

sign :: Parser String
sign = string "-" <|> string "+" <|> string ""

parseKey :: Parser INIParser
parseKey = do
    k <- many1 alphaNum
    return $ INIKey k

parseSectionName :: Parser INIParser
parseSectionName = do
    sn <- between (lexeme $ char '[') (lexeme $ char ']') (many1 alphaNum)
    return $ INISectionName sn

-- INCOMPLETE: allow all characters
parseString :: Parser INIParser
parseString = do
    char '"'
    s <- many1 alphaNum
    char '"'
    return $ INIString s

parseBareString :: Parser INIParser
parseBareString = do
    s <- many1 (noneOf "\n")
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
    k <- lexeme parseKey
    lexeme $ char '='
    v <- lexeme parseValue
    return $ INIKeyValue (k, v)

parseComment :: Parser INIParser
parseComment = do
    lexeme $ char ';'
    c <- many $ noneOf "\n"
    return $ INIComment c

parseLine :: Parser INIParser
parseLine = try parseComment <|> parseKeyValue

parseSectionKeyValues :: Parser INIParser
parseSectionKeyValues = do
    sec <- parseSectionName
    kvs <- many1 parseLine
    return $ INISection (sec, kvs)

parsePlainConfig :: Parser INIParser
parsePlainConfig = do
    kvs <- many1 parseLine
    return $ INIConfig kvs

parseINI :: Parser INIParser
parseINI = do
    p <- many (try parseSectionKeyValues <|> parsePlainConfig)
    return $ INI p

lexeme :: Parser a -> Parser a
lexeme parser = spaces *> parser <* spaces
