module Parser where

import Text.ParserCombinators.Parsec

data INIParser = INIKey String
               | INIString String
               | INIInteger Integer
               | INIDouble Double
               | INIBool Bool 
               | INISection (String, [(String, INIParser)]) deriving (Eq, Show)
