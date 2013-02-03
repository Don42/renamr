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

data Path = Path { pathToFile :: String
                 , fileName :: String
                 , fileExtension :: String
                 } deriving (Show)

main :: IO()
main = do
    args <- getArgs
    if args /= []
    then do
        let ( flags, nonOpts, msgs ) = getOpt RequireOrder [] args
        print $ parsePaths nonOpts
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
<<<<<<< HEAD

findNewFileNames :: [Path] -> [(Path, Path)]
findNewFileNames input = map regexFileName $ input

regexFileName :: Path -> (Path, Path)
regexFileName old = (old, new)
                    where new = old
=======
>>>>>>> parent of 1075058... Implements function to contain the regex parsing
