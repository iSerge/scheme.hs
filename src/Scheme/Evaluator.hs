-----------------------------------------------------------------------------
--
-- Module      :  Scheme.Evaluator
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
--{#- LANGUAGE ScopedTypeVariables -#}

module Scheme.Evaluator (
    eval
) where

import Scheme.SExpression

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
-- Special forms
eval (List [Atom "if", cond , true, false]) = case eval cond of
        Bool True -> eval true
        Bool False -> eval false
-- Normal evaluation
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", isSymbol),
    ("boolean?", isBoolean),
    ("string?", isString),
    ("string->symbol", str2atom),
    ("symbol->string", atom2str)
    ]

isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

atom2str [Atom val] = String val
atom2str _ = undefined

str2atom [String val] = Atom val
str2atom _ = undefined

isString [String _] = Bool True
isString _ = Bool False

isBoolean [Bool _] = Bool True
isBoolean _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--unpackNum (String n) = let parsed = reads n in
--                          if null parsed
--                            then 0
--                            else fst $ head parsed
--unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
