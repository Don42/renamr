#!/usr/bin/env python3
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

"""renamr

Rename TV Series Episodes
Files have to be sorted into Folders, like "SeriesName/Season/File.ext" only
the Seriesname is relevant. It is used to query epguides.com, so it should be
identical to the naemof the series on epguides. Spaces in the foldername are
ignored  when quering epguides.

Usage:
    renamr.py [options] [-v...] [--verbose...] [-q...] [--quiet] -
    renamr.py [options] [-v...] [--verbose...] [-q...] [--quiet] <file>...

Options:
    -n <name>, --name <name>    Define SeriesName to use
    -v --verbose   Increase verbosity
    -q --quite     Reduce verbosity
"""

import bs4
import csv
import docopt as dopt
import io
import os
import re
import requests
import sys


regexes = ["[S|s](\d{2})[E|e|-|_](\d{2})[^\d]",
           "[S|s](\d{2})(\d{2})[^p]",
           "(\d{2})(\d{2})[^p]",
           "(\d{1})(\d{2})[^p^\d]"]

cache = {}
verbosityLevel = 1


class NoRegexMatchException(ValueError):
    pass


def get_series_name(filename):
    """Gets the seriesname from the directory structure"""
    absPath = os.path.abspath(filename)
    series_name = os.path.basename(os.path.dirname(os.path.dirname(absPath)))
    return series_name


def get_identifier(filename):
    """Tries multiple regexes to get season and episodenumber"""
    for regex in regexes:
        match = re.search(regex, os.path.basename(filename))
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


def get_csv(series_name):
    """Return a String containing the complete CSV of a show
       Before making any requests to epguides the function checks if we already
       downloaded the csv. If yes it returns it from cache. If not it request
       the showpage parses it for the csv link gets that and returns it
    """
    url = "http://epguides.com/common/exportToCSV.asp"
    host = "epguides.com"
    short_name = series_name.replace(" ", "")
    if short_name not in cache:
        try:
            session = requests.Session()
            response = session.get('http://epguides.com/{name}/'.format(
                name=short_name))
            if response.status_code != 200:
                debug_print(0, ("The server couldn't fulfill the request."
                            " Error code: {code}").format(response.status_code))
                raise
            response.encoding = 'utf-8'
            soup = bs4.BeautifulSoup(response.text)
            soup_res = soup.find('a', href=re.compile(url))
            if soup_res is None:
                raise Exception("""Link not found in Site: {host_}/{name}/
                                \nTry specifing the name with --name.
                                """.format(host_=host, name=short_name))
            response = session.get(soup_res.get('href'))
            if response.status_code != 200:
                debug_print(0, ("The server couldn't fulfill the request."
                            " Error code: {code}").format(response.status_code))
                raise
            response.encoding = 'utf-8'
            soup = bs4.BeautifulSoup(response.text)
            cache[short_name] = soup.find('pre').contents[0].strip()
        except requests.exceptions.RequestException as e:
            debug_print(0, "Unknown error: {}".format(e))
            raise

    csvText = io.StringIO(cache[short_name])
    return csvText


def get_episode_name(ident, series_name, data_provider=get_csv):
    """Gets the Episode name from epguides"""
    try:
        reader = csv.reader(
            data_provider(series_name),
            delimiter=',',
            quotechar='"')
        for line in reader:
            if not line[0] == 'number':
                if int(line[1]) == int(ident[0]) and (int(line[2]) ==
                                                      int(ident[1])):
                    return line[5]
    except IndexError:
        pass
    return ""


def get_partial_path(path):
    """Shortens the path for output. Returns only last two folderlevels
    and the filename"""
    return os.path.join(os.path.basename(os.path.dirname(os.pathdirname(path))),
                        os.path.basename(os.path.dirname(path)),
                        os.path.basename(path))


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
    clean = re.sub(r"[\\\:\*\?\"\<\>\|]", "",
                   "".join([new_name, os.path.splitext(old_path)[1]]))
    new_path = os.path.join(os.path.dirname(old_path), clean)
    return new_path


def rename_file(old_path, new_path):
    """Rename file if it does not already exist"""
    if os.path.exists(new_path):
        debug_print(2, "File {new} already exists".format(
            new=os.path.relpath(new_path)))
    else:
        os.rename(old_path, new_path)
    debug_print(1, "\"{old}\"|\"{new}\"".format(
        old=get_partial_path(old_path),
        new=get_partial_path(new_path)))


def read_files_from_args(fileList):
    files = filter(os.path.exists, fileList)
    absFiles = map(os.path.realpath, map(os.path.normpath, files))
    return absFiles


def read_files_from_file(path):
    fileList = [line.replace("\n", "") for line in path]
    return read_files_from_args(fileList)


def main(args):
    global verbosityLevel
    verbosityLevel = (args["--verbose"] + 1 - args["--quite"])

    absFiles = []
    if(not args["-"]):
        absFiles = read_files_from_args(args["<file>"])
    else:
        absFiles = read_files_from_file(sys.stdin)

    for file in absFiles:
        debug_print(3, "Operating on File {filename}".format(filename=file))
        series_name = ""
        if(not args["--name"]):
            series_name = get_series_name(file)
        else:
            series_name = args["--name"]
        debug_print(3, "Using Seriesname {name}".format(name=series_name))

        try:
            ident = get_identifier(file)
        except NoRegexMatchException:
            debug_print(
                0, "Error: No regex match on file {file_}. Skipping".format(
                    file_=file))
            continue

        ep_name = get_episode_name(ident, series_name)

        new_path = make_new_path(series_name,
                                 ident,
                                 ep_name,
                                 file)
        debug_print(3, "New path: {new}".format(new=new_path))
        rename_file(file, new_path)
    else:
        debug_print(3, "Done processing all files")
        sys.exit(0)


if __name__ == "__main__":
    arguments = dopt.docopt(__doc__)
    main(arguments)
