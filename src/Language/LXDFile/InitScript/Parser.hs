module Language.LXDFile.InitScript.Parser where

import Data.Either.Combinators (mapLeft)

import Text.Parsec
import Text.Parsec.String (Parser)

import Language.LXDFile.InitScript.Lexer
import Language.LXDFile.InitScript.Types
import Language.LXDFile.Parser (arguments, untilEol, eol, contents, normalize)

parseString :: String -> Either InitScriptError InitScript
parseString s = mapLeft ParseError (parseStringAST s) >>= initScript

parseFile :: FilePath -> IO (Either InitScriptError InitScript)
parseFile fp = do
    ast' <- parseFileAST fp
    return $ mapLeft ParseError ast' >>= initScript

parseStringAST :: String -> Either ParseError AST
parseStringAST = parse (contents ast) "(string)" . normalize

parseFileAST :: FilePath -> IO (Either ParseError AST)
parseFileAST fp = parse (contents ast) fp . normalize <$> readFile fp

instruction :: Parser Instruction
instruction = try cd
          <|> try comment
          <|> try copy
          <|> try run
          <|> try eolInstruction

comment :: Parser Instruction
comment = char '#' *> (Comment <$> untilEol)

cd :: Parser Instruction
cd = reserved "CD" *> (Action . ChangeDirectory <$> untilEol)

copy :: Parser Instruction
copy = do
    reserved "COPY"
    src <- many (noneOf " \t")
    whiteSpace
    dst <- untilEol
    return $ Action $ Copy src dst

env :: Parser Instruction
env = do
    reserved "ENV"
    key <- many (noneOf "= \t")
    whiteSpace
    value <- untilEol
    return $ Action $ Environment key value

eolInstruction :: Parser Instruction
eolInstruction = eol *> pure EOL

run :: Parser Instruction
run = reserved "RUN" *> (Action . Run <$> arguments)

ast :: Parser AST
ast = many $ do
    pos <- getPosition
    i <- instruction
    optional eol
    return $ InstructionPos i (sourceName pos) (sourceLine pos)
