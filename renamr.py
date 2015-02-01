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
    renamr.py [options] -
    renamr.py [options] <file>...

Options:
    -n <name>, --name <name>    Define SeriesName to use
    -v --verbose   Increase verbosity
    -q --quiet     Reduce verbosity
    -d --dry-run   Don't actually rename files
"""

import collections
import bs4
import csv
import docopt as dopt
import io
import logging
import pathlib as pl
import re
import requests
import sys

logger = logging.getLogger('renamr')
logger.setLevel(logging.INFO)

regexes = ['[S|s](\d{2})[E|e|-|_](\d{2})[^\d]',
           '[S|s](\d{2})(\d{2})[^p]',
           '(\d{2})(\d{2})[^p]',
           '(\d{1})(\d{2})[^p^\d]']

cache = {}

EpisodeIdent = collections.namedtuple('EpisodeIdent', ['season', 'episode'])


class NoRegexMatchException(ValueError):
    pass


def get_series_name(file_path):
    """Gets the seriesname from the directory structure

    Args:
        file_path (pathlib.Path): Path to the file

    Returns:
        (string): Name of the series

    """
    series_name = file_path.parts[-3]
    return series_name


def get_identifier(file_path):
    """Tries multiple regexes to get season and episode number

    Args:
        filename (pathlib.Path): Absolute path to file

    Returns:
        (EpisodeIdent): Identifing the episode

    """
    for regex in regexes:
        match = re.search(regex, file_path.name)
        if match is not None:
            ident = EpisodeIdent(int(match.groups()[0]),
                                 int(match.groups()[1]))
            return ident
    raise NoRegexMatchException()


def build_identifier(identifier):
    """Builds the episode identifier, consisting of season and episodenumber

    Args:
        identifier (EpisodeIdent): Identifier of a episode

    Returns:
        (string): Identifing the episode

    """
    if identifier.season < 0 or identifier.episode < 0:
        raise ValueError("Season and Episode can't be negative")
    ret = 'S{ident.season:>02}E{ident.episode:>02}'.format(ident=identifier)
    return ret


def get_csv(series_name):
    """Return a String containing the complete CSV of a show

       Before making any requests to epguides the function checks if we already
       downloaded the csv. If yes it returns it from cache. If not it request
       the showpage parses it for the csv link gets that and returns it

    Args:
        series_name (string): Name of the series

    Returns:
        (string): csv of the series

    """
    url = 'http://epguides.com/common/exportToCSV.asp'
    host = 'epguides.com'
    short_name = series_name.replace(' ', '')
    if short_name not in cache:
        try:
            session = requests.Session()
            response = session.get('http://epguides.com/{name}/'.format(
                name=short_name))
            if response.status_code != 200:
                logger.error(("The server couldn't fulfill the request."
                              " Error code: {code}").format(
                                  response.status_code))
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
                logger.error(("The server couldn't fulfill the request."
                              " Error code: {code}").format(
                                  response.status_code))
                raise
            response.encoding = 'utf-8'
            soup = bs4.BeautifulSoup(response.text)
            cache[short_name] = soup.find('pre').contents[0].strip()
        except requests.exceptions.RequestException as e:
            logger.error("Unknown error: {}".format(e))
            raise

    csvText = io.StringIO(cache[short_name])
    return csvText


def get_episode_name(ident, series_name, data_provider=get_csv):
    """Gets the Episode name from epguides

    Args:
        ident (EpisodeIdent): Episode identifier
        series_name (string): Name of the series
        data_provider (function): Function that takes the episode name and
            returns a string containing csv of the series

    Returns:
        (string): Episode name or empty string

    """
    try:
        reader = csv.reader(
            data_provider(series_name),
            delimiter=',',
            quotechar='"')
        for line in reader:
            if not line[0] == 'number':
                if int(line[1]) == ident.season and (int(line[2]) ==
                                                     ident.episode):
                    return line[5]
    except IndexError:
        pass
    return ""


def get_partial_path(path):
    """Shortens the path for output.

    Returns only last two folderlevels
    and the filename

    Args:
        path (pathlib.Path): Path to a file

    Returns:
        (list): containing the last three path elements

    """
    return path.parts[-3:]


def make_new_path(series_name, ident, ep_name, old_path):
    """Generate new path for file

    Args:
        series_name (string): Name of the series
        ident (EpisodeIdent): Identifier of the episode
        ep_name (string): Name of the episode
        old_path (pathlib.Path): Path to the file

    Returns:
        (pathlib.Path): new file path

    """
    if None in (series_name, ident, old_path):
        raise ValueError("Parameters can't be 'None'")
    new_name = "{series} {ident_} - {epname}{ext}".format(
        series=series_name,
        ident_=build_identifier(ident),
        epname=ep_name,
        ext=old_path.suffix)
    return old_path.with_name(new_name)


def rename_file(old_path, new_path):
    """Rename file if it does not already exist

    Args:
        old_path (pathlib.Path): Path to the file to be renamed
        new_path (pathlib.Path): Path the file should be renamed to

    """
    if new_path.exists():
        logger.warning("File {new} already exists".format(
            new=new_path))
    else:
        old_path.rename(new_path)
    logger.info("\"{old}\"|\"{new}\"".format(
        old=get_partial_path(old_path),
        new=get_partial_path(new_path)))


def read_files_from_args(path_list):
    """Read files from list and check existens

    Args:
        file_list (list): List of files to check and make absolute

    Returns:
        list: of existing, absolute paths
    """
    files = filter(pl.Path.exists, path_list)
    abs_paths = map(pl.Path.resolve, files)
    return abs_paths


def read_files_from_file(file):
    """Read files from the provided file

    Args:
        file (file object): Every line in this file is treated as one file to
            be renamed. This is usually sys.stdin

    Returns:
        list: of existing, absolute paths
    """
    file_list = [pl.Path(line.replace("\n", "")) for line in file]
    return read_files_from_args(file_list)


def main():
    args = dopt.docopt(__doc__)
    if args['--quiet']:
        logger.setLevel(logging.ERROR)
    if args['--verbose']:
        logger.setLevel(logging.DEBUG)

    absFiles = []
    if(not args['-']):
        absFiles = read_files_from_args([pl.Path(x) for x in args['<file>']])
    else:
        absFiles = read_files_from_file(sys.stdin)

    for file_path in absFiles:
        logger.debug("Operating on File {filename}".format(filename=file_path))
        series_name = ''
        if(not args['--name']):
            series_name = get_series_name(file_path)
        else:
            series_name = args['--name']
        logger.debug("Using Seriesname {name}".format(name=series_name))

        try:
            ident = get_identifier(file_path)
        except NoRegexMatchException:
            logger.error(
                "Error: No regex match on file {file_}. Skipping".format(
                    file_=file_path))
            continue

        ep_name = get_episode_name(ident, series_name)

        new_path = make_new_path(series_name,
                                 ident,
                                 ep_name,
                                 file_path)
        logger.debug("New path: {new}".format(new=new_path))
        if not args.get('--dry-run', False):
            rename_file(file_path, new_path)
    else:
        logger.debug("Done processing all files")
        sys.exit(0)


if __name__ == '__main__':
    main()
