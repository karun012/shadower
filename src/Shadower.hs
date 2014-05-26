{-# OPTIONS -Wall #-}
 
module Main where

import Filesystem.Path.CurrentOS (fromText, encodeString)
import Data.Text (pack)
import Data.String.Utils

import Control.Monad (void)

import System.FSNotify
import System.Environment
import System.Cmd
import System.Exit

main :: IO ()
main = getArgs >>= getFilePathFromArgs >>= watchPath

getFilePathFromArgs :: [String] -> IO FilePath
getFilePathFromArgs (filePath:_) = return filePath
getFilePathFromArgs []       = return "."

watchPath :: FilePath -> IO ()
watchPath path = withManager $ \manager -> do
       _ <- watchTree manager (fromText $ pack path) (const True) handler
       _ <- getLine
       exitSuccess

handler :: Event -> IO()
handler action = case action of 
                     Modified file _ -> runDocTests (encodeString file)
                     _ -> return ()

runDocTests :: String -> IO ()
runDocTests file = case isHaskellSource file of 
                   True -> void $ system $ "echo Running doctests in " ++ file ++ "&& doctest -isrc " ++ file
                   _ -> return ()

isHaskellSource :: String -> Bool
isHaskellSource = endswith ".hs"
