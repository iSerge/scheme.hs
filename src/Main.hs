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

import Scheme.Parser
import Scheme.Evaluator

main:: IO ()
main =  getArgs >>= print . eval . readExpr . head 

