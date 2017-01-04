renamr
======

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/e9a1a8b1a7474e9d9f583e0b62fb586b)](https://www.codacy.com/app/donmarco42/renamr?utm_source=github.com&utm_medium=referral&utm_content=Don42/renamr&utm_campaign=badger)

Script to rename series episodes

Usage
-----

If files are sorted into folders, either like "Seriesname/file.ext" or "SeriesName/Season 01/File.ext",
the series name is used for the files. If the files are not sorted or the series name can't be recognized
the name has to be specified on the command line. This limits the script to one series per run.
The series name is used to query *tvmaze.com*, so it should be identical to the name of the series on that site.

If folders are specified, all mkv files inside that folder are added to the list of files to be processed.
