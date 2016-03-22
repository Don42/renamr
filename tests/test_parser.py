import pathlib
import unittest

from ddt import data, unpack, ddt, file_data

import renamr.parser


no_regex_data = [
    "",
    "asiudiv.720p.mkv",
    "./siudfuc.1080p.mkv",
    "/media/Some Show(2005)/Season 01/ixc.720p.1080p.mkv"
]


@ddt
class RegexTest(unittest.TestCase):
    @file_data("regex_test_data.json")
    @unpack
    def test_get_identifier(self, pair):
        name, result = pair
        assert tuple(result) == renamr.parser.get_identifier(pathlib.Path(name)).identifier()

    @data(*no_regex_data)
    def test_get_identifier_Error(self, name):
        with self.assertRaises(renamr.parser.NoRegexMatchException):
            renamr.parser.get_identifier(pathlib.Path(name))


def test_get_series_name_abs_path():
    assert "Some Show" == renamr.parser.get_series_name(pathlib.Path("/Some Show/Season 1/bubl.mkv"))


def test_get_series_name_rel_path():
    assert "Some Show" == renamr.parser.get_series_name(pathlib.Path("Some Show/Season 1/blbub"))
