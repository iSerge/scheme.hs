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

import Control.Monad

import Scheme.SExpression
import Scheme.Exception

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
-- Special forms
eval (List [Atom "if", cond , true, false]) = case eval cond of
        Right (Bool True) -> eval true
        Right (Bool False) -> eval false
eval (List ((Atom "if") : params)) = throwError $ NumArgs 3 params
-- Normal evaluation
eval (List (Atom func : args)) = mapM eval args >>=  apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                                        ($ args)
                                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

atom2str [Atom val] = return $ String val
atom2str notAtom = throwError $ TypeMismatch "number" $ List notAtom

str2atom [String val] = return $ Atom val
str2atom notString = throwError $ TypeMismatch "number" $ List notString

isString [String _] = return $ Bool True
isString _ = return $ Bool False

isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = liftM (Number . foldl1 op) (mapM unpackNum params)
--mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal ->  ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
