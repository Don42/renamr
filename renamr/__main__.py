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
import sys

import docopt
from renamr.processor import create_file_list, process_files

logger = logging.getLogger('renamr')
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

ch = logging.StreamHandler()
ch.setFormatter(formatter)
ch.setLevel(logging.DEBUG)

logger.setLevel(logging.INFO)
logger.addHandler(ch)


def main():
    args = docopt.docopt(__doc__)
    if args['--quiet']:
        logger.setLevel(logging.ERROR)
    if args['--verbose']:
        logger.setLevel(logging.DEBUG)

    abs_files = create_file_list(sys.stdin) if args['-'] else create_file_list(args['<file>'])

    process_files(abs_files,
                  series_name=args['--name'],
                  dry_run=args.get('--dry-run', False),
                  cache_file=args.get('--cache', None))

    logger.info("Done processing all files")


if __name__ == '__main__':
    main()

