{-# OPTIONS -Wall #-}
 
module Main where

import ShadowerFunctions
import System.Environment
import Control.Applicative

main :: IO ()
main = getFilePathFromArgs <$> getArgs >>= watchPath
