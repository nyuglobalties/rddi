## Release summary

Minor release to 

* include mixed content XML elements (those that allow a child element and a value) and updated applicable package functions
* reorder DDI elements to automatically match the DDI Codebook 2.5 schema
* remove NA attribute values from functions which streamlines codebook generation for the user
* update vignette and README with examples to handle mixed content DDI elements
* general bug fixes

## Test Environments

* local OS X install, R 4.2.0
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)
* win-builder(release & devel)

## R CMD check results

There were no ERRORS or WARNINGS.

There are two NOTES. 

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.  In addition, I can't replicate the issue on my local build.

❯ On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

HTML Validation occurs locally on MAC OS and the note doesn't appear in the winbuilder-release, winbuilder-devel, R-hub windows-x86_64-devel (r-devel), or R-hub ubuntu-gcc-release (r-release). It can probably be safely ignored. 
