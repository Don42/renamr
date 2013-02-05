-- ----------------------------------------------------------------------------
-- "THE SCOTCH-WARE LICENSE" (Revision 42):
-- <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a scotch whisky in return
-- Marco 'don' Kaulea
-- ----------------------------------------------------------------------------
import System.Environment
import System.FilePath
import System.IO
import System.Console.GetOpt
import System.Exit

import Text.Regex.Posix

data Path = Path { pathToFile :: String
                 , fileName :: String
                 , fileExtension :: String
                 } deriving (Show)

regex1 = "[S|s][0-9]{2}[E|e|-|_][0-9]{2}"
regex2 = "[0-9]{4}[^p]"
regex3 = "[0-9]{3}[^p]"


main :: IO()
main = do
    args <- getArgs
    if args /= []
    then do
        let ( flags, nonOpts, msgs ) = getOpt RequireOrder [] args
        let renameOps = findNewFileNames $ parsePaths nonOpts
        mapM_ print renameOps
    else do
        print "usage: renamr filename [..]"
        exitWith $ ExitFailure 1

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
                                , fileName = regexFileName $ fileName old
                                , fileExtension = fileExtension old
                                }

regexFileName :: String -> String
regexFileName old
    | old =~ regex1 = old =~ regex1
    | old =~ regex2 = old =~ regex2
    | old =~ regex3 = old =~ regex3
    | otherwise     = old

