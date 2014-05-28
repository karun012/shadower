{-# OPTIONS -Wall #-}
 
module Main where

import Filesystem.Path.CurrentOS (fromText, encodeString, dirname)
import Filesystem.Path (parent)
import Data.Text (pack)
import Data.String.Utils

import Control.Exception
import qualified Control.Exception as E

import System.FSNotify
import System.Environment
import System.Exit

import Test.DocTest

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
                 Modified file _ -> maybeRunDocTests (encodeString file)
                 _ -> return ()

maybeRunDocTests :: String -> IO ()
maybeRunDocTests file = case isHaskellSource file of 
                   True -> E.catch (runDocTests file) ignoreAllExceptions
                   _ -> return ()

ignoreAllExceptions :: SomeException -> IO ()
ignoreAllExceptions _ = return ()

runDocTests :: String -> IO ()
runDocTests file = do
              _ <- putStrLn $ "Running doctests in " ++ file
              doctest $ generateAllSourceLocations file ++ [file]
generateAllSourceLocations :: String -> [String]
generateAllSourceLocations "/" = []
generateAllSourceLocations xs = (generateAllSourceLocations $ encodeString $ parent $ fromText $ pack xs) ++ ["-i" ++ (encodeString $ parent $ fromText $ pack xs)]
isHaskellSource :: String -> Bool
isHaskellSource file = endswith ".hs" file || endswith ".lhs" file
