import pathlib as pl
import re

from renamr.series_database import EpisodeIdentifier

regexes = ['[S|s](?P<series>\d{2})[E|e|-|_](?P<episode>\d{2})[^\d]',
           '[S|s](?P<series>\d{2})(?P<episode>\d{2})[^p]',
           '(?P<series>\d{2})(?P<episode>\d{2})[^p]',
           '(?P<series>\d{1})(?P<episode>\d{2})[^p^\d]']


class NoRegexMatchException(ValueError):
    pass


def get_series_name(file_path: pl.Path) -> str:
    """Gets the seriesname from the directory structure

    :param file_path: Path to the file
    :type file_path: (pathlib.Path)
    :return: Name of the series
    :rtype: str
    """
    parts = file_path.parts
    if "season" in parts[-2].casefold():
        series_name = parts[-3]
    else:
        series_name = parts[-2]
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
            ident = EpisodeIdentifier(int(match.group('series')),
                                      int(match.group('episode')))
            return ident
    raise NoRegexMatchException()

