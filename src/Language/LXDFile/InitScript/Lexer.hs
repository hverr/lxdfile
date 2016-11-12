module Language.LXDFile.InitScript.Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser, makeTokenParser)
import qualified Text.Parsec.Token as Token

lexer :: TokenParser ()
lexer = makeTokenParser languageDef

reserved :: String -> Parser ()
reserved = Token.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

languageDef :: LanguageDef st
languageDef = emptyDef { Token.reservedNames = reservedNames }

reservedNames :: [String]
reservedNames = [ "CD"
                , "COPY"
                , "ENV"
                , "ONUPDATE"
                , "RUN"
                ]
