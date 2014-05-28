{-# OPTIONS -Wall #-}
 
module Main where

import Filesystem.Path.CurrentOS (fromText, encodeString)
import Filesystem.Path (parent)
import Data.Text (pack)
import Data.String.Utils

import Control.Exception
import qualified Control.Exception as E
import Control.Applicative
import Safe (headMay)

import System.FSNotify
import System.Environment
import System.Exit

import Test.DocTest

main :: IO ()
main = getFilePathFromArgs <$> getArgs >>= watchPath

getFilePathFromArgs :: [String] -> String
getFilePathFromArgs = maybe "." id . headMay

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
              doctest $ everyPossibleSourceLocationFromRoot file ++ [file]

everyPossibleSourceLocationFromRoot :: String -> [String]
everyPossibleSourceLocationFromRoot "/" = []
everyPossibleSourceLocationFromRoot currentFolder = (everyPossibleSourceLocationFromRoot $ parentFolderAsString currentFolder) ++ ["-i" ++ (parentFolderAsString currentFolder)]

parentFolderAsString :: String -> String
parentFolderAsString = encodeString . parent . fromText . pack

isHaskellSource :: String -> Bool
isHaskellSource file = endswith ".hs" file || endswith ".lhs" file
