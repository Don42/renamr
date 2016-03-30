import logging
import pathlib as pl

from renamr.series_database import EpisodeIdentifier

logger = logging.getLogger(__name__)


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


def make_new_path(series_name: str, ident: EpisodeIdentifier, ep_name: str, old_path: pl.Path):
    """Generate new path for file

    :param series_name: Name of the series
    :type series_name: str
    :param ident: Identifier of the episode
    :type ident: EpisodeIdentifier
    :param ep_name: Name of the episode
    :type ep_name: str
    :param old_path: Path to the file
    :type old_path: pathlib.Path

    :return: new file path
    :rtype: pathlib.Path

    """
    if None in (series_name, ident, old_path):
        raise ValueError("Parameters can't be 'None'")
    new_name = "{series} {ident} - {epname}{ext}".format(
        series=series_name,
        ident=ident,
        epname=ep_name,
        ext=old_path.suffix)
    return old_path.with_name(new_name)

