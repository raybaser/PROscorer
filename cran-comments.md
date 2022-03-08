## Test environments
* Local Windows 10 install, R 4.1.2
* Ubuntu 20.04.4 LTS (on github actions), devel, release, oldrel-1 
* Mac OS 11.6.4 (on github actions), devel, release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* The 1 note is a false positive regarding a "(possibly) invalid file URI". I obtained this note only in the win-builder checks. My local and github actions checks had 0 notes.  
* This release addresses CRAN check errors needed to retain this package on CRAN.
* My email address is correct.  

## Reverse dependencies

This package has no reverse dependencies.  
