#!/usr/bin/env python3
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

import io
import unittest
import renamr


class test_renamr(unittest.TestCase):

    def test_build_identifier(self):
        self.assertEqual("S01E01", renamr.build_identifier((1, 1)))
        self.assertEqual("S10E20", renamr.build_identifier((10, 20)))
        self.assertEqual("S11E999", renamr.build_identifier((11, 999)))
        self.assertEqual("S00E00", renamr.build_identifier((0, 0)))

    def test_build_identifier_errors(self):
        with self.assertRaises(TypeError):
            renamr.build_identifier(None)
        with self.assertRaises(Exception):
            renamr.build_identifier(15)
        with self.assertRaises(ValueError):
            renamr.build_identifier((-1, 5))
        with self.assertRaises(ValueError):
            renamr.build_identifier((1, -5))

    def test_get_series_name_abs_path(self):
        self.assertEqual("Some Show", renamr.get_series_name(
            "/Some Show/Season 1/bubl.mkv"))

    def test_get_series_name_rel_path(self):
        self.assertEqual("Some Show", renamr.get_series_name(
            "Some Show/Season 1/blbub"))

    def test_get_identifier_SxxExx(self):
        self.assertEqual((1, 1), renamr.get_identifier(
            "abfldi_S01E01.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "abfldi_s11E20.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "abfldi_S99e29.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "abfldi_s10_80.720p.mkv"))
        self.assertEqual((1, 1), renamr.get_identifier(
            "./abfldi_S01E01.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "./abfldi_s11E20.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "Some Show/Season 01/abfldi_S99e29.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "/Some Show/Season 1/abfldi_s10_80.720p.mkv"))

    def test_get_identifier_Sxxxx(self):
        self.assertEqual((1, 1), renamr.get_identifier(
            "abfldi_S0101.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "bfldi_S1120.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "bfldi_S9929.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "asduih_s1080.720p.mkv"))
        self.assertEqual((1, 1), renamr.get_identifier(
            "./abfldi_S0101.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "./Some Show/Season 2/bfldi_S1120.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "../Some Show/Season 22/bfldi_S9929.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "/media/Some Show/Season 13/asduih_s1080.720p.mkv"))

    def test_get_identifier_xxxx(self):
        self.assertEqual((1, 1), renamr.get_identifier(
            "abfldi_0101.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "abfldi_1120.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "abfldi_9929.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "asduih_1080.720p.mkv"))
        self.assertEqual((11, 99), renamr.get_identifier(
            "assdf_1199_1080p.mkv"))
        self.assertEqual((9, 14), renamr.get_identifier(
            "asiud_1080p.0914.mkv"))
        self.assertEqual((1, 1), renamr.get_identifier(
            "/media/Some Show/Season 01/abfldi_0101.720p.mkv"))
        self.assertEqual((11, 20), renamr.get_identifier(
            "./abfldi_1120.720p.mkv"))
        self.assertEqual((99, 29), renamr.get_identifier(
            "../abfldi_9929.720p.mkv"))
        self.assertEqual((10, 80), renamr.get_identifier(
            "../Some Show/asduih_1080.720p.mkv"))
        self.assertEqual((11, 99), renamr.get_identifier(
            "../Some Show/Season 1/assdf_1199_1080p.mkv"))
        self.assertEqual((9, 14), renamr.get_identifier(
            "../Some Show/Season 03/asiud_1080p.0914.mkv"))

    def test_get_identifier_xxx(self):
        self.assertEqual((1, 1), renamr.get_identifier(
            "abfldi_101.720p.mkv"))
        self.assertEqual((1, 20), renamr.get_identifier(
            "abfldi_120.720p.mkv"))
        self.assertEqual((9, 29), renamr.get_identifier(
            "abfldi_929.720p.mkv"))
        self.assertEqual((9, 14), renamr.get_identifier(
            "asiud_720p.914.mkv"))
        self.assertEqual((1, 1), renamr.get_identifier(
            "./abfldi_101.720p.mkv"))
        self.assertEqual((1, 20), renamr.get_identifier(
            "../abfldi_120.720p.mkv"))
        self.assertEqual((9, 29), renamr.get_identifier(
            "/media/Some Show/Season 199/abfldi_929.720p.mkv"))
        self.assertEqual((9, 14), renamr.get_identifier(
            "../../media/Some Show/Season 21/asiud_720p.914.mkv"))

    def test_get_identifier_Error(self):
        with self.assertRaises(renamr.NoRegexMatchException):
            renamr.get_identifier("")
        with self.assertRaises(renamr.NoRegexMatchException):
            renamr.get_identifier("asiudiv.720p.mkv")
        with self.assertRaises(renamr.NoRegexMatchException):
            renamr.get_identifier("./siudfuc.1080p.mkv")
        with self.assertRaises(renamr.NoRegexMatchException):
            renamr.get_identifier(
                "/media/Some Show(2005)/Season 01/ixc.720p.1080p.mkv")

    def test_get_episode_name(self):
        data = lambda x: io.StringIO("""number,blub,stuff,psps,blbub,name\n001,5,16,516,,"Felina"\n""")
        ret = renamr.get_episode_name((5, 16), "Breaking Bad", data)
        self.assertEqual("Felina", ret)


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(test_renamr))
    return suite

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite())
