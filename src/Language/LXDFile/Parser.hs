module Language.LXDFile.Parser where

import Control.Monad (void)
import Data.Either.Combinators (mapLeft)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Text.Parsec
import Text.Parsec.String (Parser)

import Language.LXDFile.Lexer
import Language.LXDFile.Types

parseString :: String -> Either LXDFileError LXDFile
parseString s = mapLeft ParseError (parseStringAST s) >>= mapLeft ASTError . lxdFile

parseFile :: FilePath -> IO (Either LXDFileError LXDFile)
parseFile fp = do
    ast <- parseFileAST fp
    return $ mapLeft ParseError ast >>= mapLeft ASTError . lxdFile

parseStringAST :: String -> Either ParseError AST
parseStringAST = parse (contents ast) "(string)" . normalize

parseFileAST :: FilePath -> IO (Either ParseError AST)
parseFileAST fp = parse (contents ast) fp . normalize <$> readFile fp

instrunction :: Parser Instruction
instrunction = try comment
           <|> try copy
           <|> try from
           <|> try run
           <|> try eolInstruction

comment :: Parser Instruction
comment = char '#' *> (Comment <$> untilEol)

copy :: Parser Instruction
copy = do
    reserved "COPY"
    src <- many (noneOf " \t")
    whiteSpace
    dst <- untilEol
    return $ Action $ Copy src dst

eolInstruction :: Parser Instruction
eolInstruction = eol *> pure EOL

from :: Parser Instruction
from = reserved "FROM" *> (From <$> untilEol)

run :: Parser Instruction
run = reserved "RUN" *> (Action . Run <$> arguments)

arguments :: Parser Arguments
arguments = try argumentsList <|> try argumentsShell

argumentsList :: Parser Arguments
argumentsList = ArgumentsList <$> brackets (commaSep stringLiteral)

argumentsShell :: Parser Arguments
argumentsShell = ArgumentsShell <$> untilEol

ast :: Parser AST
ast = many $ do
    pos <- getPosition
    i <- instrunction
    optional eol
    return $ InstructionPos i (sourceName pos) (sourceLine pos)

untilEol :: Parser String
untilEol = many (noneOf "\n")

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' >> option '\n' (char '\n'))

contents :: Parser a -> Parser a
contents p = whiteSpace *> p <* eof

normalize :: String -> String
normalize = removePlaceholders . compensateLinebreaks . replaceEscapedNewlines . trimLines
  where
    replace old new = intercalate new . splitOn old
    occurences s x = length (splitOn x s) - 1
    replaceEscapedNewlines = replace "\\\n" "\\\\"
    removePlaceholders = replace "\\\\" " "
    compensateLinebreaks = concatMap compensate . lines

    compensate :: String -> String
    compensate line = line ++ "\n" ++ generateLinebreaks line

    generateLinebreaks :: String -> String
    generateLinebreaks line = concat . replicate (occurences line "\\\\") $ "\n"


trimLines :: String -> String
trimLines = unlines . map strip . lines
  where
    strip = lstrip . rstrip
    lstrip = dropWhile (`elem` [' ', '\t'])
    rstrip = reverse . lstrip . reverse
