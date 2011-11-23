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

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces:: Parser ()
spaces = skipMany1 space

parseString:: Parser LispVal
parseString = do
    char '"'
    x <- many(noneOf "\"")
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
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"




