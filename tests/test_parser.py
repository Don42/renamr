import pathlib
import unittest

from ddt import data, unpack, ddt

import renamr.parser

identifier_test_data_1 = [
    ("abfldi_S01E01.720p.mkv", (1, 1)),
    ("abfldi_s11E20.720p.mkv", (11, 20)),
    ("abfldi_S99e29.720p.mkv", (99, 29)),
    ("abfldi_s10_80.720p.mkv", (10, 80)),
    ("./abfldi_S01E01.720p.mkv", (1, 1)),
    ("./abfldi_s11E20.720p.mkv", (11, 20)),
    ("Some Show/Season 01/abfldi_S99e29.720p.mkv", (99, 29)),
    ("/Some Show/Season 1/abfldi_s10_80.720p.mkv", (10, 80))
]

identifier_test_data_2 = [
    ("abfldi_S0101.720p.mkv", (1, 1)),
    ("bfldi_S1120.720p.mkv", (11, 20)),
    ("bfldi_S9929.720p.mkv", (99, 29)),
    ("asduih_s1080.720p.mkv", (10, 80)),
    ("./abfldi_S0101.720p.mkv", (1, 1)),
    ("./Some Show/Season 2/bfldi_S1120.720p.mkv", (11, 20)),
    ("../Some Show/Season 22/bfldi_S9929.720p.mkv", (99, 29)),
    ("/media/Some Show/Season 13/asduih_s1080.720p.mkv", (10, 80))
]

identifier_test_data_3 = [
    ("abfldi_0101.720p.mkv", (1, 1)),
    ("abfldi_1120.720p.mkv", (11, 20)),
    ("abfldi_9929.720p.mkv", (99, 29)),
    ("asduih_1080.720p.mkv", (10, 80)),
    ("assdf_1199_1080p.mkv", (11, 99)),
    ("asiud_1080p.0914.mkv", (9, 14)),
    ("/media/Some Show/Season 01/abfldi_0101.720p.mkv", (1, 1)),
    ("./abfldi_1120.720p.mkv", (11, 20)),
    ("../abfldi_9929.720p.mkv", (99, 29)),
    ("../Some Show/asduih_1080.720p.mkv", (10, 80)),
    ("../Some Show/Season 1/assdf_1199_1080p.mkv", (11, 99)),
    ("../Some Show/Season 03/asiud_1080p.0914.mkv", (9, 14))
]

identifier_test_data_4 = [
    ("abfldi_101.720p.mkv", (1, 1)),
    ("abfldi_120.720p.mkv", (1, 20)),
    ("abfldi_929.720p.mkv", (9, 29)),
    ("asiud_720p.914.mkv", (9, 14)),
    ("./abfldi_101.720p.mkv", (1, 1)),
    ("../abfldi_120.720p.mkv", (1, 20)),
    ("/media/Some Show/Season 199/abfldi_929.720p.mkv", (9, 29)),
    ("../../media/Some Show/Season 21/asiud_720p.914.mkv", (9, 14))
]

no_regex_data = [
    "",
    "asiudiv.720p.mkv",
    "./siudfuc.1080p.mkv",
    "/media/Some Show(2005)/Season 01/ixc.720p.1080p.mkv"
]


@ddt
class ParserTest(unittest.TestCase):
    @data(*identifier_test_data_1)
    @unpack
    def test_get_identifier_SxxExx(self, name, result):
        assert result == renamr.parser.get_identifier(pathlib.Path(name)).identifier()

    @data(*identifier_test_data_2)
    @unpack
    def test_get_identifier_Sxxxx(self, name, result):
        assert result == renamr.parser.get_identifier(pathlib.Path(name)).identifier()

    @data(*identifier_test_data_2)
    @unpack
    def test_get_identifier_xxxx(self, name, result):
        assert result == renamr.parser.get_identifier(pathlib.Path(name)).identifier()

    @data(*identifier_test_data_4)
    @unpack
    def test_get_identifier_xxx(self, name, result):
        assert result == renamr.parser.get_identifier(pathlib.Path(name)).identifier()

    @data(*no_regex_data)
    def test_get_identifier_Error(self, name):
        with self.assertRaises(renamr.parser.NoRegexMatchException):
            renamr.parser.get_identifier(pathlib.Path(name))


def test_get_series_name_abs_path():
    assert "Some Show" == renamr.parser.get_series_name(pathlib.Path("/Some Show/Season 1/bubl.mkv"))


def test_get_series_name_rel_path():
    assert "Some Show" == renamr.parser.get_series_name(pathlib.Path("Some Show/Season 1/blbub"))
