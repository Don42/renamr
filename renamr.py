#!/usr/bin/env python
# ----------------------------------------------------------------------------
# "THE SCOTCH-WARE LICENSE" (Revision 42):
# <DonMarco42@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a scotch whisky in return
# Marco 'don' Kaulea
# ----------------------------------------------------------------------------

import re
import sys
import unicodecsv
from os.path import exists, abspath, dirname, basename
from urllib2 import urlopen
from BeautifulSoup import BeautifulSoup
from cStringIO import StringIO
regexes = ["[S|s](\d{2})[E|e|-|_](\d{2})", "[S|s](\d{2})(\d{2})[^p]",
           "(\d{2})(\d{2})",  "(\d{1})(\d{2})[^p]"]

cache = {}


def getSeriesName(fileName):
    absPath = abspath(fileName)
    seriesName = basename(dirname(dirname(absPath)))
    return seriesName


def getIdentifier(fileName):
    for regex in regexes:
        match = re.search(regex, basename(fileName))
        if match is not None:
            ident = (int(match.groups()[0]), int(match.groups()[1]))
            return ident
    raise Exception("NoRegexMatch")


def buildIdentifer(identifier):
    return "S%02dE%02d" % identifier


def getEpisodeName(seriesName, ident):
    shortName = seriesName.replace(" ", "")
    url = "http://epguides.com/common/exportToCSV.asp"
    if shortName not in cache:
        con = urlopen("http://epguides.com/%s" % shortName)
        soup = BeautifulSoup(con.read())
        link = soup.find('a', href=re.compile(url)).get("href")
        con.close()
        csvCon = urlopen(link)
        soup = BeautifulSoup(csvCon.read())
        csvText = StringIO(soup.find('pre').contents[0].strip())
        cache[shortName] = csvText

    reader = unicodecsv.reader(cache[shortName],  delimiter=',', encoding='utf-8')
    reader.next()
    for line in reader:
        if int(line[1]) == int(ident[0]) and int(line[2]) == int(ident[1]):
            return line[5]
    return ""


def main(argv):
    files = filter(exists, argv)
    for file in files:
        seriesName = getSeriesName(file)
        ident = getIdentifier(file)
        epName = getEpisodeName(seriesName, ident)
        print "%s %s - %s" % (seriesName, buildIdentifer(ident), epName)

if __name__ == "__main__":
    main(sys.argv[1:])
