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
import docopt as dopt
import json
import logging
import pathlib as pl
import re
import requests
import sys

ENCODING = 'utf-8'

SINGLESEARCH_SHOWS_URL = 'http://api.tvmaze.com/singlesearch/shows'

logger = logging.getLogger('renamr')
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

ch = logging.StreamHandler()
ch.setFormatter(formatter)
ch.setLevel(logging.DEBUG)

logger.setLevel(logging.INFO)
logger.addHandler(ch)

regexes = ['[S|s](\d{2})[E|e|-|_](\d{2})[^\d]',
           '[S|s](\d{2})(\d{2})[^p]',
           '(\d{2})(\d{2})[^p]',
           '(\d{1})(\d{2})[^p^\d]']

cache = {}

EpisodeIdent = collections.namedtuple('EpisodeIdent', ['season', 'episode'])


class NoRegexMatchException(ValueError):
    pass


def get_series_name(file_path: pl.Path) -> str:
    """Gets the seriesname from the directory structure

    :param file_path: Path to the file
    :type file_path: (pathlib.Path)
    :return: Name of the series
    :rtype: str
    """
    series_name = file_path.parts[-3]
    return series_name


def get_identifier(file_path: pl.Path) -> EpisodeIdent:
    """Tries multiple regexes to get season and episode number

    :param file_path: Absolute path to the file
    :type file_path: pathlib.Path
    :returns: Identifing the episode
    :rtype: EpisodeIdent
    """
    for regex in regexes:
        match = re.search(regex, file_path.name)
        if match is not None:
            ident = EpisodeIdent(int(match.groups()[0]),
                                 int(match.groups()[1]))
            return ident
    raise NoRegexMatchException()


def build_identifier(identifier: EpisodeIdent) -> str:
    """Builds the episode identifier, consisting of season and episode number

    :param identifier: Identifier of a episode
    :type identifier: EpisodeIdent
    :returns: String identifier for the episode
    :rtype: str
    """
    if identifier.season < 0 or identifier.episode < 0:
        raise ValueError("Season and Episode can't be negative")
    ret = 'S{ident.season:>02}E{ident.episode:>02}'.format(ident=identifier)
    return ret


def download_series_page(short_name: str) -> str:
    """

    :param short_name: Short name of a series
    :return: Downloaded page
    """
    session = requests.Session()
    payload = {'q': short_name, 'embed': 'episodes'}
    response = session.get(SINGLESEARCH_SHOWS_URL, params=payload)
    if response.status_code == 200:
        response.encoding = ENCODING
        return response.text
    elif response.status_code == 404:
        raise NameError  # TODO Create proper exception
    else:
        logger.error("The server couldn't fulfill the request. Error code: {code}".format(response.status_code))
        raise requests.RequestException


def extract_episode_name_mapping(series_page):
    """

    :param series_page: Page to parse
    :return: mapping from season/episode to name
    """
    content = json.loads(series_page)
    episodes = content['_embedded']['episodes']
    ident_name_mapping = dict()
    for episode in episodes:
        ident_name_mapping[EpisodeIdent(int(episode['season']), int(episode['number']))] = episode['name']
    return ident_name_mapping


def get_series_data(series_name):
    """

    :param series_name: Name of the series
    :return: mapping for the series
    """
    short_name = series_name.replace(' ', '-')
    if short_name not in cache:
        page = download_series_page(short_name)
        cache[short_name] = extract_episode_name_mapping(page)
    return cache[short_name]


def get_episode_name(ident, series_data):
    """

    :param ident: Identifier for the episode
    :param series_data: mapping for the series
    :return: episode name
    """
    try:
        return series_data[ident]
    except IndexError:
        return ""


def get_partial_path(path: pl.Path) -> list:
    """Shortens the path for output.

    Returns only last two folderlevels
    and the filename

    :param path: Path to a file
    :type path: pathlib.Path
    :return: last three path elements
    :rtype: list

    """
    return path.parts[-3:]


def make_new_path(series_name: str, ident: EpisodeIdent, ep_name: str, old_path: pl.Path):
    """Generate new path for file

    :param series_name: Name of the series
    :type series_name: str
    :param ident: Identifier of the episode
    :type ident: EpisodeIdent
    :param ep_name: Name of the episode
    :type ep_name: str
    :param old_path: Path to the file
    :type old_path: pathlib.Path

    :return: new file path
    :rtype: pathlib.Path

    """
    if None in (series_name, ident, old_path):
        raise ValueError("Parameters can't be 'None'")
    new_name = "{series} {ident_} - {epname}{ext}".format(
        series=series_name,
        ident_=build_identifier(ident),
        epname=ep_name,
        ext=old_path.suffix)
    return old_path.with_name(new_name)


def rename_file(old_path: pl.Path, new_path: pl.Path):
    """Rename file if it does not already exist

    :param old_path: Path to the file to be renamed
    :type old_path: pathlib.Path
    :param new_path: Path the file should be renamed to
    :type new_path: pathlib.Path

    """
    if new_path.exists():
        logger.warning("File {new} already exists".format(
            new=new_path))
    else:
        old_path.rename(new_path)
    logger.info("\"{old}\"|\"{new}\"".format(
        old=get_partial_path(old_path),
        new=get_partial_path(new_path)))


def create_file_list(source: list) -> list:
    """Read files from list and check existence

    :param source: List of files to check and make absolute
    :type source: list

    :return: existing, absolute paths in a list
    :rtype: list
    """
    path_list = [pl.Path(x.replace('\n', '')) for x in source]
    files = filter(pl.Path.exists, path_list)
    abs_paths = map(pl.Path.resolve, files)
    return abs_paths


def main():
    args = dopt.docopt(__doc__)
    if args['--quiet']:
        logger.setLevel(logging.ERROR)
    if args['--verbose']:
        logger.setLevel(logging.DEBUG)

    abs_files = create_file_list(sys.stdin) if args['-'] else create_file_list(args['<file>'])

    for file_path in abs_files:
        logger.info("Operating on File {filename}".format(filename=file_path))

        series_name = args['--name'] or get_series_name(file_path)
        logger.debug("Using Seriesname {name}".format(name=series_name))

        try:
            ident = get_identifier(file_path)
        except NoRegexMatchException:
            logger.error("Error: No regex match on file {file_}. Skipping".format(file_=file_path))
            continue

        series_data = get_series_data(series_name)
        ep_name = get_episode_name(ident, series_data)

        new_path = make_new_path(series_name, ident, ep_name, file_path)
        logger.info("New path: {new}".format(new=new_path))
        if not args.get('--dry-run', False):
            rename_file(file_path, new_path)
    else:
        logger.debug("Done processing all files")
        sys.exit(0)


if __name__ == '__main__':
    main()
