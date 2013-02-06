-- ----------------------------------------------------------------------------
-- "THE SCOTCH-WARE LICENSE" (Revision 42):
-- <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a scotch whisky in return
-- Marco 'don' Kaulea
-- ----------------------------------------------------------------------------
module Main(main) where

import System.Environment
import System.FilePath
import System.IO
import System.Console.GetOpt
import System.Directory(getCurrentDirectory)
import System.Exit

import Data.Char

import Text.Regex.Posix


data Path = Path { pathToFile :: String
                 , fileName :: String
                 , fileExtension :: String
                 }

instance Show Path where
    show path = combine (pathToFile path) $ name ++ ext
                where
                    name = fileName path
                    ext = fileExtension path

regex1 = "[S|s][0-9]{2}[E|e|-|_][0-9]{2}"
regex2 = "[0-9]{4}[^p]"
regex3 = "[0-9]{3}[^p]"


-- |Reads file names as arguments, parses and outputs them to use in awk/mv
main :: IO()
main = do
    args <- getArgs
    absPath <- getCurrentDirectory
    if args /= [] 
        then do
            let ( flags, nonOpts, msgs ) = getOpt RequireOrder [] args
            let absPaths = map (combine absPath) nonOpts
            let renameOps = findNewFileNames $ parsePaths absPaths
            mapM_ putStrLn $ map (buildPathPairs) renameOps
        else do
            print "usage: renamr filename [..]"
            exitWith $ ExitFailure 1

-- |Parses one String to one Path Type
parsePath :: String -> Path
parsePath input = Path { pathToFile = takeDirectory input
                       , fileName = takeBaseName input
                       , fileExtension = takeExtension input
                       }

											 
parsePaths :: [String] -> [Path]
parsePaths input = map parsePath $ input

findNewFileNames :: [Path] -> [(Path, Path)]
findNewFileNames input = map regexPath $ input

regexPath :: Path -> (Path, Path)
regexPath old = (old, new)
                where
                    new = Path { pathToFile = pathToFile old
                                , fileName = seriesName ++ " " ++ identifier
                                , fileExtension = fileExtension old
                                }
                    seriesName
                        | length reverseSplitPath >= 2 = (reverseSplitPath) !! 1
                        | otherwise = takeWhile (/='.') $ fileName old
                    reverseSplitPath = reverse $ splitDirectories $ pathToFile old
                    identifier = buildIdentifier $ regexFileName $ fileName old
                    buildIdentifier numbers
                        | length numbers == 4 = "S" ++
                                                (take 2 numbers)
                                                ++ "E" ++
                                                (drop 2 numbers)
                        | length numbers == 3 = "S0" ++
                                                (take 1 numbers)
                                                ++ "E" ++
                                                (drop 1 numbers)
                        | otherwise = numbers
	
regexFileName :: String -> String
regexFileName old
    | old =~ regex1 = filter isDigit $ old =~ regex1
    | old =~ regex2 = filter isDigit $ old =~ regex2
    | old =~ regex3 = filter isDigit $ old =~ regex3
    | otherwise     = old

buildPathPairs :: (Path, Path) -> String
buildPathPairs (old, new) = "\"" ++ (show old) ++ "\"|\"" ++ (show new) ++ "\""
