{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Main where

import GH (decodeJSON)
import LocusGPX (pathToGPX)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

main = do
    [fn] <- getArgs
    f <- B.readFile fn
    let Right gpx = do
            [p] <- decodeJSON f
            return $ pathToGPX p
    putStrLn gpx
