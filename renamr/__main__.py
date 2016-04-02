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
If files are sorted into folders, either like "Seriesname/file.ext" or "SeriesName/Season 01/File.ext",
the series name is used for the files. If the files are not sorted or the series name can't be recognized
the name has to be specified on the command line. This limits the script to one series per run.
The series name is used to query *tvmaze.com*, so it should be identical to the name of the series on that site.

If folders are specified, all mkv files inside that folder are added to the list of files to be processed.

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

