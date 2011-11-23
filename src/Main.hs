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

import Scheme.Parser
import System.Environment

main:: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)


