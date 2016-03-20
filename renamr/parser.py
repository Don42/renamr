import pathlib as pl
import re

from renamr.series_database import EpisodeIdentifier

regexes = ['[S|s](\d{2})[E|e|-|_](\d{2})[^\d]',
           '[S|s](\d{2})(\d{2})[^p]',
           '(\d{2})(\d{2})[^p]',
           '(\d{1})(\d{2})[^p^\d]']


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


def get_identifier(file_path: pl.Path) -> EpisodeIdentifier:
    """Tries multiple regexes to get season and episode number

    :param file_path: Absolute path to the file
    :type file_path: pathlib.Path
    :returns: Identifing the episode
    :rtype: EpisodeIdentifier
    """
    for regex in regexes:
        match = re.search(regex, file_path.name)
        if match is not None:
            ident = EpisodeIdentifier(int(match.groups()[0]),
                                      int(match.groups()[1]))
            return ident
    raise NoRegexMatchException()

