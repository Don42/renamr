#!/usr/bin/env python3
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

import pathlib as pl
import unittest
import renamr.series_database


class test_renamr(unittest.TestCase):
    def test_get_episode_name(self):
        data = {renamr.EpisodeIdent(5, 16): "Felina"}
        ret = renamr.get_episode_name(
            renamr.EpisodeIdent(5, 16),
            data)
        self.assertEqual("Felina", ret)


def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(test_renamr))
    return suite

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite())
