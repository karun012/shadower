{-# LANGUAGE ScopedTypeVariables #-}

module ShadowerFunctions
(getFilePathFromArgs, watchPath)
where
 
import Filesystem.Path.CurrentOS (fromText, encodeString)
import Filesystem.Path (parent)
import Data.Text (pack)
import Data.String.Utils

import Control.Exception
import qualified Control.Exception as E
import Safe (headMay)
import Control.Monad.Writer

import System.FSNotify
import System.Exit

import Test.DocTest

-- | Gets the file path from arguments
-- If there are no arguments, then it returns "." (the current folder)
--
-- >>> getFilePathFromArgs []
-- "."
--
-- >>> getFilePathFromArgs ["/aFolder"]
-- "/aFolder"
--
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
                   True -> do 
                      let header = colorize $ "Running doctests in " ++ file 
                      let headerLength = length header
                      let footer = colorize $ (take headerLength . repeat) '*' ++ "\n"
                      _ <- putStrLn $ header 
                      _ <- runDocTests file :: IO (Either E.SomeException ())
                      putStrLn $ footer 
                   _ -> return ()

runDocTests :: Exception e => String -> IO (Either e ())
runDocTests file = (try . doctest) (everyPossibleSourceLocationFromRoot file ++ [file])

-- | Returns a list of -i<folders> recursively from root 
-- to the parent of the current folder
-- 
-- >>> everyPossibleSourceLocationFromRoot "/home"
-- ["-i/"]
--
-- >>> everyPossibleSourceLocationFromRoot "/usr/share"
-- ["-i/","-i/usr/"]
everyPossibleSourceLocationFromRoot :: String -> [String]
everyPossibleSourceLocationFromRoot "/" = []
everyPossibleSourceLocationFromRoot currentFolder = (everyPossibleSourceLocationFromRoot $ parentFolderAsString currentFolder) ++ ["-i" ++ (parentFolderAsString currentFolder)]

-- | Gets the string representation of the 
-- parent folder for a given folder
--
-- >>> parentFolderAsString "/home"
-- "/"
parentFolderAsString :: String -> String
parentFolderAsString = encodeString . parent . fromText . pack

-- | Checks if the file type is .hs or .lhs
--
-- >>> isHaskellSource "wat.bleh"
-- False
--
-- >>> isHaskellSource "wat.hs"
-- True
--
-- >>> isHaskellSource "wat.lhs"
-- True
isHaskellSource :: String -> Bool
isHaskellSource file = endswith ".hs" file || endswith ".lhs" file

colorize :: String -> String
colorize string = "\x1b[32m" ++ string ++ "\x1b[0m"
