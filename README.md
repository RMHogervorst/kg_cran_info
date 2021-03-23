## Downloading all CRAN package information

This is a project to collect all CRAN package, find their dependencies
and exported functions and use that information to build a knowledge graph.

This program does only two things: 

1. download all tar.gz files of packages, extract information and remove that tarball again. 
2. extract information from the files and dump them into a sqlite database

## 1. Download all packages and extract information
The end result is a set of folders, one folder for each package and three
files per package. And the man pages.

For instance :
```
dplyr-0.1.1
----NAMESPACE
----DESCRIPTION
----INFO
----MAN/
```

The main script to retrieve all information from
CRAN to local files is [extract_entries.R](extract_entries.R)

## 2.  extract information from the files and dumping into the db.

The main script for extracting file information into a 
database is [write_pgk_info_to_db.R](write_pkg_info_to_db.R)

Once done the hashes can be exported to files with [export_data.R](export_data.R)

# Notes
Because there are many many files, this set of scripts takes several days to run. I ran it on a spare laptop I had lying around. I had to manually restart some too. I think I made it more robust, but it still sometimes fails. 
Once we have a stable base, additions of data through cranberries will be way faster.  Because I only have to download and parse a few files a day. 

* new version: download new version and parse
* archived package: nothing
* new package: download new version

I could add the base R packages from the R versions too. But that would require me downloading the entire R tarball, extracting a few packages, renaming a few files, replacing some texts. I'll leave it for future consideration.
