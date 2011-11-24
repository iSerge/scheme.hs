-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import System.Environment
import Control.Monad

import Scheme.Parser
import Scheme.Evaluator
import Scheme.Exception

main:: IO ()
--main =  getArgs >>= print . eval . readExpr . head
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
 

