#!/usr/bin/env python3
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

import unittest
import renamr


class test_renamr(unittest.TestCase):

    def test_build_identifier(self):
        self.assertEqual("S01E01", renamr.buildIdentifer((1, 1)))
        self.assertEqual("S10E20", renamr.buildIdentifer((10, 20)))
        self.assertEqual("S11E999", renamr.buildIdentifer((11, 999)))
        self.assertEqual("S00E00", renamr.buildIdentifer((0, 0)))
        self.assertRaises(renamr.buildIdentifer(None))
        self.assertRaises(renamr.buildIdentifer(15))
        self.assertRaises(renamr.buildIdentifer((-1, 5)))
        self.assertRaises(renamr.buildIdentifer((1, -5)))

    def test_get_series_name_abs_path(self):
        self.assertEqual("Arctic Air", renamr.getSeriesName("/Arctic Air/Season 1/bubl.mkv"))

    def test_get_series_name_rel_path(self):
        self.assertEqual("Arctic Air", renamr.getSeriesName("Arctic Air/Season 1/blbub"))


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(test_renamr))
    return suite

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite())
