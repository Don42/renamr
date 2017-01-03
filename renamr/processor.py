import logging
import pathlib as pl

from renamr.file_handling import make_new_path, rename_file
from renamr.parser import get_series_name, get_identifier, NoRegexMatchException
from renamr.series_database import SeriesDatabase

logger = logging.getLogger(__name__)


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
    abs_paths = [x.resolve() for x in pure_files]
    return abs_paths


def process_files(abs_files, series_name=None, dry_run=False, cache_file=None):
    db = SeriesDatabase(cache_file)
    for file_path in abs_files:
        logger.info("Operating on File %s", file_path.name)

        series_name = series_name or get_series_name(file_path)
        logger.debug("Using Seriesname %s", series_name)

        try:
            ident = get_identifier(file_path)
        except NoRegexMatchException:
            logger.error("Error: No regex match on file %s. Skipping", file_path.name)
            continue

        ep_name = db.get_episode_name(series_name, ident)

        new_path = make_new_path(series_name, ident, ep_name, file_path)
        logger.info("New path: %s", new_path.name)
        if not dry_run:
            rename_file(file_path, new_path)
        else:
            print("{}|{}".format(file_path, new_path))

