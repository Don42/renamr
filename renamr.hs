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
import Text.Printf
import Text.HTML.TagSoup
import Text.StringLike
import qualified Data.ByteString.Lazy as L

import Network.URI
import Network.HTTP
import Network.Browser

data Path = Path { pathToFile :: String
                 , fileName :: String
                 , fileExtension :: String
                 }

data Episode = Episode { name :: String
                       , season :: Int
                       , episode :: Int
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
            -- Parse Commandline Options
            let ( flags, nonOpts, msgs ) = getOpt RequireOrder [] args
            -- Add current path to relative paths used to make them absolute
            let absPaths = map (combine absPath) nonOpts
            -- Do the actual parsing
            let episodes = findNewFileNames $ parsePaths absPaths

            stuff <- getSeriesSite "ArcticAir"

            print stuff

            -- Start getting the episode data from epguides
            let renameOps = map getEpisodeNames episodes
            -- Output all filename pairs
            mapM_ (putStrLn . buildPathPairs) renameOps
        else do
            print "usage: renamr filename [..] \n Files should be already sorted into folders first by series then by season"
            exitWith $ ExitFailure 1

getSeriesSite :: String -> IO [Tag String]
getSeriesSite seriesName = do
        (_, rsp) <- Network.Browser.browse $ do
                setOutHandler $ const (return ())
                setAllowRedirects True -- handle HTTP redirects
                request $ getRequest ("http://epguides.com/" ++ seriesName)
        let tags =  parseTags (fromString (rspBody rsp))
        let navbar = takeWhile (~/= "</div>") $
                 dropWhile (~/= "<div id=\"topnavbar\"") tags
        let link = takeWhile (~/= "</a>") $
                dropWhile (~/= "<a onclick") navbar
        return link

-- | Parses one String to one Path Type
parsePath :: String -> Path
parsePath input = Path { pathToFile = takeDirectory input
                       , fileName = takeBaseName input
                       , fileExtension = takeExtension input
                       }

getEpisodeNames :: (Path, Episode) -> (Path, Path)
getEpisodeNames (old_in, new_in) = (old_out, new_out)
                    where
                        old_out = old_in
                        new_out = Path { pathToFile = pathToFile old_in
                                       , fileName = getFullName new_in
                                       , fileExtension = fileExtension old_in
                                       }

getFullName :: Episode -> String
getFullName ep = series ++ seasonNo ++ episodeNo
            where
                series = name ep
                seasonNo = printf " S%02d" $ season ep
                episodeNo = printf "E%02d" $ episode ep


-- | Parse a list of strings to a list of paths
parsePaths :: [String] -> [Path]
parsePaths = map parsePath

-- | Use regexPath on all elements of a list of Paths
findNewFileNames :: [Path] -> [(Path, Episode)]
findNewFileNames = map regexPath

-- | Take one Path type and return a pair containing the old Path and the created new Path
regexPath :: Path -> (Path, Episode)
regexPath old = (old, new)
                where
                    new = Episode { name = seriesName
                                  , season   = fst identifier
                                  , episode  = snd identifier
                                  }
                    seriesName
                        | length reverseSplitPath >= 2 = reverseSplitPath !! 1
                        | otherwise = takeWhile (/='.') $ fileName old
                    reverseSplitPath = reverse . splitDirectories $ pathToFile old
                    identifier = readIdentifier $ regexFileName $ fileName old
                    readIdentifier numbers
                        | length numbers == 4 = (read $ take 2 numbers,
                                                read $ drop 2 numbers)
                        | length numbers == 3 = (read $ take 1 numbers,
                                                read $ drop 1 numbers)
                        | otherwise = (0, 0)

-- | Check which regex fits best and use that result
regexFileName :: String -> String
regexFileName old
    | old =~ regex1 = filter isDigit $ old =~ regex1
    | old =~ regex2 = filter isDigit $ old =~ regex2
    | old =~ regex3 = filter isDigit $ old =~ regex3
    | otherwise     = old

-- | Add the to filenames and paths together and format them to be read be awk
buildPathPairs :: (Path, Path) -> String
buildPathPairs (old, new) = "\"" ++ show old ++ "\"|\"" ++ show new ++ "\""
