#!/usr/bin/env python3
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

import re
import sys
import csv
import argparse
from os.path import exists, abspath, dirname, basename, join, splitext,\
    normpath, realpath, relpath
from os import rename
from http.client import HTTPConnection, HTTPException, InvalidURL
from bs4 import BeautifulSoup
from io import StringIO
from urllib import parse


regexes = ["[S|s](\d{2})[E|e|-|_](\d{2})", "[S|s](\d{2})(\d{2})[^p]",
           "(\d{2})(\d{2})",  "(\d{1})(\d{2})[^p]"]

cache = {}
verbosityLevel = 1


def getSeriesName(fileName):
    """Gets the seriesname from the directory structure"""
    absPath = abspath(fileName)
    seriesName = basename(dirname(dirname(absPath)))
    return seriesName


def getIdentifier(fileName):
    """Tries multiple regexes to get season and episodenumber"""
    for regex in regexes:
        match = re.search(regex, basename(fileName))
        if match is not None:
            ident = (int(match.groups()[0]), int(match.groups()[1]))
            return ident
    raise Exception("NoRegexMatch")


def buildIdentifer(identifier):
    """Builds the episode identifier, consisting of season and episodenumber"""
    return "S%02dE%02d" % identifier


def getEpisodeName(seriesName, ident):
    """Gets the Episode name from epguides"""
    shortName = seriesName.replace(" ", "")
    try:
        reader = csv.reader(getCsv(shortName),  delimiter=',')
        for line in reader:
            if not line[0] == 'number':
                if int(line[1]) == int(ident[0]) and int(line[2]) == int(ident[1]):
                    return line[5]
    except Exception as e:
        debugPrint(1, e.reason())
    return ""


def getCsv(shortName):
    """Return a String containing the complete CSV of a show
       Before making any requests to epguides the function checks if we already
       downloaded the csv. If yes it returns it from cache. If not it request
       the showpage parses it for the csv link gets that and returns it
    """
    url = "http://epguides.com/common/exportToCSV.asp"
    host = "epguides.com"
    if shortName not in cache:
        try:
            con = HTTPConnection(host)
            con.request("GET", "/%s/" % shortName)
            soup = BeautifulSoup(con.getresponse())
            soup_res = soup.find('a', href=re.compile(url))
            if soup_res is None:
                raise Exception("Link not found in Site: %s/%s/shortName"
                                % (host, shortName))
            link = parse.urlparse(soup_res.get("href"))
            con.request("GET", "%s?%s" % (link.path, link.query))
            soup = BeautifulSoup(con.getresponse())
            cache[shortName] = soup.find('pre').contents[0].strip()
            con.close()

        except HTTPException as e:
            debugPrint(0, 'The server couldn\'t fulfill the request.\n'
                       'Error code: %s' % e.code)
            sys.exit(1)
        except InvalidURL as e:
            debugPrint(0, 'We failed to reach a server.\nReason: %s' % e.reason)
            sys.exit(1)
    csvText = StringIO(cache[shortName])
    return csvText


def getPartialPath(path):
    """Shortens the path for output. Returns only last two folderlevels
    and the filename"""
    return join(basename(dirname(dirname(path))), basename(dirname(path)),
                basename(path))


def debugPrint(verbosity, message):
    if(verbosityLevel >= verbosity):
        print(message)


def main(argv):
    parser = setupArgsParser()
    args = parser.parse_args()
    global verbosityLevel
    verbosityLevel = (args.verbose + 1)

    files = filter(exists, args.filenames)
    absFiles = map(realpath, map(normpath, files))
    for file in absFiles:
        debugPrint(2, "Operating on File %s" % file)
        seriesName = getSeriesName(file)
        debugPrint(2, "Found Seriesname %s" % seriesName)
        try:
            ident = getIdentifier(file)
        except Exception as e:
            debugPrint(0, "Error %s: No regex match on file %s" % (e, file))
            continue
        epName = getEpisodeName(seriesName, ident)
        newName = "%s %s - %s" % (seriesName, buildIdentifer(ident), epName)
        clean = re.sub(r"[\\\:\*\?\"\<\>\|]", "", newName + splitext(file)[1])
        newPath = join(dirname(file), clean)

        if exists(newPath):
            debugPrint(1, "File %s already exists" % relpath(newPath))
        else:
            rename(file, newPath)
        debugPrint(1, "\"%s\"|\"%s\"" % (getPartialPath(file), getPartialPath(newPath)))


def setupArgsParser():
    parser = argparse.ArgumentParser(description="""Rename TV Series Episodes
                                     Files have to be sorted into Folders, like
                                     "SeriesName/Season/File.ext" only the
                                     Seriesname is relevant. It is used to query
                                     epguides.com. So it should be identical to the
                                     name of the series on epguides. Spaces
                                     in the foldername are ignored when quering
                                     epguides.""")
    parser.add_argument('filenames', nargs='+', help='paths to the episodes')
    parser.add_argument("-v", "--verbose", help="increase output verbosity",
                        action="count")
    return parser


if __name__ == "__main__":
    main(sys.argv[1:])
