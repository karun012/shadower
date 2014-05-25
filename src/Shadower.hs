{-# OPTIONS -Wall #-}
 
module Main where

import Filesystem.Path.CurrentOS (fromText, encodeString)
import Data.Text (pack)
import Control.Monad (void)
import System.FSNotify
import System.Environment
import System.Cmd
import System.Exit
import Data.String.Utils

main :: IO ()
main = getArgs >>= getFilePathFromArgs >>= run

getFilePathFromArgs ::  [String] -> IO FilePath
getFilePathFromArgs []       = return "."
getFilePathFromArgs (filePath:_) = return filePath

run :: FilePath -> IO ()
run path = withManager $ \m -> do
       _ <- watchTree m (fromText $ pack path) (const True) handler
       _ <- getLine
       exitSuccess

handler :: Event -> IO()
handler action = case action of 
                     Modified file _ -> runDocTests (encodeString file)
                     _ -> return ()
runDocTests :: String -> IO ()
runDocTests file = case isHaskellSource file of 
                   True -> void $ system $ "doctest -isrc " ++ file
                   _ -> return ()

isHaskellSource :: String -> Bool
isHaskellSource = endswith ".hs"
