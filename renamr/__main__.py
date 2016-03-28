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
    --cache <filename> Filename to use as cache
"""

import logging
import pathlib as pl
import sys

import docopt

from renamr.file_handling import rename_file, make_new_path
from renamr.parser import NoRegexMatchException, get_series_name, get_identifier
from renamr.series_database import SeriesDatabase

logger = logging.getLogger('renamr')
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

ch = logging.StreamHandler()
ch.setFormatter(formatter)
ch.setLevel(logging.DEBUG)

logger.setLevel(logging.INFO)
logger.addHandler(ch)


def create_file_list(source: list) -> list:
    """Read files from list and check existence

    :param source: List of files to check and make absolute
    :type source: list

    :return: existing, absolute paths in a list
    :rtype: list
    """
    path_list = [pl.Path(x.replace('\n', '')) for x in source]
    files = [x for x in path_list if x.exists()]
    pure_files = list()
    for path in files:
        if path.is_dir():
            sub_files = path.glob('*.mkv')
            pure_files.extend(sub_files)
        else:
            pure_files.append(path)
    abs_paths = (x.resolve() for x in pure_files)
    return abs_paths


def main():
    args = docopt.docopt(__doc__)
    if args['--quiet']:
        logger.setLevel(logging.ERROR)
    if args['--verbose']:
        logger.setLevel(logging.DEBUG)

    abs_files = create_file_list(sys.stdin) if args['-'] else create_file_list(args['<file>'])

    db = SeriesDatabase(args.get('--cache', None))

    for file_path in abs_files:
        logger.info("Operating on File {filename}".format(filename=file_path))

        series_name = args['--name'] or get_series_name(file_path)
        logger.debug("Using Seriesname {name}".format(name=series_name))

        try:
            ident = get_identifier(file_path)
        except NoRegexMatchException:
            logger.error("Error: No regex match on file {file_}. Skipping".format(file_=file_path))
            continue

        ep_name = db.get_episode_name(series_name, ident)

        new_path = make_new_path(series_name, ident, ep_name, file_path)
        logger.info("New path: {new}".format(new=new_path))
        if not args.get('--dry-run', False):
            rename_file(file_path, new_path)
    else:
        logger.debug("Done processing all files")
        sys.exit(0)


if __name__ == '__main__':
    main()
