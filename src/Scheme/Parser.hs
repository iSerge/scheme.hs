-----------------------------------------------------------------------------
--
-- Module      :  Scheme.Parser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Scheme.Parser (
    readExpr
) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Scheme.SExpression
import Scheme.Exception


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces:: Parser ()
spaces = skipMany1 space

escapedChar = do
    char '\\'
    c <- letter <|> oneOf "\\\"" --oneOf "\\\"nrt"
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        'r' -> return '\r'
        _   -> return c

parseStringChar = escapedChar <|> noneOf "\"\\"

parseString:: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> noneOf "\"")
    char '"'
    return $ String x

parseAtom:: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many(letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber:: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr:: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

parseChar :: Parser LispVal
parseChar = undefined

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return  val



