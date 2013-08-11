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
import argparse
import csv
from os.path import exists, abspath, dirname, basename, join, splitext,\
    normpath, realpath, relpath
from os import rename
from http.client import HTTPConnection, HTTPException, InvalidURL
from bs4 import BeautifulSoup
from io import StringIO
from urllib import parse


regexes = ["[S|s](\d{2})[E|e|-|_](\d{2})",
           "[S|s](\d{2})(\d{2})[^p]",
           "(\d{2})(\d{2})[^p]",
           "(\d{1})(\d{2})[^p^\d]"]

cache = {}
verbosityLevel = 1


class NoRegexMatchException(ValueError):
    pass


def get_series_name(filename):
    """Gets the seriesname from the directory structure"""
    absPath = abspath(filename)
    seriesName = basename(dirname(dirname(absPath)))
    return seriesName


def get_identifier(filename):
    """Tries multiple regexes to get season and episodenumber"""
    for regex in regexes:
        match = re.search(regex, basename(filename))
        if match is not None:
            ident = (int(match.groups()[0]), int(match.groups()[1]))
            return ident
    raise NoRegexMatchException()


def build_identifier(identifier):
    """Builds the episode identifier, consisting of season and episodenumber"""
    ret = "S{ident[0]:>02}E{ident[1]:>02}".format(ident=identifier)
    (season, episode) = identifier
    if season < 0 or episode < 0:
        raise ValueError("Season and Episode can't be negative")
    return ret


def get_episode_name(ident, csv_data):
    """Gets the Episode name from epguides"""
    try:
        reader = csv.reader(csv_data,  delimiter=',')
        for line in reader:
            if not line[0] == 'number':
                if int(line[1]) == int(ident[0]) and (int(line[2]) ==
                                                      int(ident[1])):
                    return line[5]
    except Exception as e:
        debug_print(1, e.reason())
    return ""


def get_csv(short_name):
    """Return a String containing the complete CSV of a show
       Before making any requests to epguides the function checks if we already
       downloaded the csv. If yes it returns it from cache. If not it request
       the showpage parses it for the csv link gets that and returns it
    """
    url = "http://epguides.com/common/exportToCSV.asp"
    host = "epguides.com"
    if short_name not in cache:
        try:
            con = HTTPConnection(host)
            con.request("GET", "/{name}/".format(name=short_name))
            soup = BeautifulSoup(con.getresponse())
            soup_res = soup.find('a', href=re.compile(url))
            if soup_res is None:
                raise Exception("""Link not found in Site: {host_}/{name}/
                                shortName""".format(host_=host, name=short_name))
            link = parse.urlparse(soup_res.get("href"))
            con.request("GET", "{path}?{query}".format(path=link.path,
                                                       query=link.query))
            soup = BeautifulSoup(con.getresponse())
            cache[short_name] = soup.find('pre').contents[0].strip()
            con.close()

        except HTTPException as e:
            debug_print(0, 'The server couldn\'t fulfill the request.\n'
                        'Error code: {code}'.format(code=e.code))
            raise
        except InvalidURL as e:
            debug_print(
                0, 'We failed to reach a server.\nReason: {reason}'.format(
                    reason=e.reason))
            raise
    csvText = StringIO(cache[short_name])
    return csvText


def get_partial_path(path):
    """Shortens the path for output. Returns only last two folderlevels
    and the filename"""
    return join(basename(dirname(dirname(path))), basename(dirname(path)),
                basename(path))


def debug_print(verbosity, message):
    """Print message if global verbosityLevel is higher than verbosity"""
    if(verbosityLevel >= verbosity):
        print(message)


def make_new_path(series_name, ident, ep_name, old_path):
    """Generate new path for file"""
    if None in (series_name, ident, old_path):
        raise ValueError("Parameters can't be 'None'")
    new_name = "{series} {ident_} - {epname}".format(
        series=series_name,
        ident_=build_identifier(ident),
        epname=ep_name)
    clean = re.sub(r"[\\\:\*\?\"\<\>\|]", "", new_name + splitext(old_path)[1])
    return join(old_path, clean)


def rename_file(old_path, new_path):
    """Rename file if it does not already exist"""
    if exists(new_path):
        debug_print(1, "File {new} already exists".format(
            new=relpath(new_path)))
    else:
        rename(old_path, new_path)
    debug_print(1, "\"{old}\"|\"{new}\"".format(
        old=get_partial_path(old_path),
        new=get_partial_path(new_path)))


def main(argv):
    parser = setupArgsParser()
    args = parser.parse_args()
    global verbosityLevel
    verbosityLevel = (args.verbose + 1)

    files = filter(exists, args.filenames)
    absFiles = map(realpath, map(normpath, files))

    for file in absFiles:
        debug_print(2, "Operating on File {filename}".format(filename=file))
        series_name = get_series_name(file)
        debug_print(2, "Found Seriesname {name}".format(name=series_name))
        try:
            ident = get_identifier(file)
        except NoRegexMatchException:
            debug_print(0, "Error: No regex match on file {file_}".format(
                file_=file))
            continue
        try:
            short_name = series_name.replace(" ", "")
            csv_data = get_csv(short_name)
        except HTTPException:
            continue
        except InvalidURL:
            continue
        ep_name = get_episode_name(ident, csv_data)
        new_path = make_new_path(series_name,
                                 ident,
                                 ep_name,
                                 dirname(file))
        rename_file(file, new_path)
    else:
        debug_print(2, "Done processing all files")
        sys.exit(0)


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
                        action="count", default=0)
    return parser


if __name__ == "__main__":
    main(sys.argv[1:])
