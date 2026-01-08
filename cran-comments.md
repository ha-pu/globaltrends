# Resubmission 0.1.0

## Test environments
* local Windows 11 pro, 64 bit (R 4.5.2)
* macOS-latest (release) on GitHub
* windows-latest (release) on GitHub
* ubuntu-latest (devel) on GitHub
* ubuntu-latest (release) on GitHub
* ubuntu-latest (oldrel-1) on GitHub
* all available platforms through R-Hub (rhub::rhub_platforms)
  1. [VM]  linux
  2. [VM]  m1-san
  3. [VM]  macos
    + The macOS-13 based runner images are now retired. For more details, see
      https://github.com/actions/runner-images/issues/13046.
  4. [VM]  macos-arm64
  5. [VM]  windows
  6. [CT]  atlas  [ATLAS]
  7. [CT]  c23  [C23]
  8. [CT]  clang-asan  [asan, clang-ASAN]
  9. [CT]  clang-ubsan  [clang-UBSAN, ubsan]
  10. [CT]  clang16  [clang16]
  11. [CT]  clang17  [clang17]
  12. [CT]  clang18  [clang18]
  13. [CT]  clang19  [clang19]
  14. [CT]  clang20  [clang20]
  15. [CT]  donttest  [donttest]
  16. [CT]  gcc-asan  [gcc-ASAN, gcc-UBSAN]
  17. [CT]  gcc13  [gcc13]
  18. [CT]  gcc14  [gcc14]
  19. [CT]  gcc15  [gcc15]
  20. [CT]  intel  [Intel]
  21. [CT]  lto  [LTO]
  22. [CT]  mkl  [MKL]
  23. [CT]  nold  [noLD]
  24. [CT]  noremap  [noRemap]
  25. [CT]  nosuggests  [noSuggests]
    + Error: processing vignette 'globaltrends.Rmd' failed with diagnostics:
      there is no package called ‘rmarkdown’ -> this error is triggered by the
      test platform and not globaltrends
  26. [CT]  rchk  [rchk]
    + Error: bcheck output file does not exist -> this error is triggered by the
      test platform and not globaltrends
  27. [CT]  ubuntu-clang  [r-devel-linux-x86_64-debian-clang]
  28. [CT]  ubuntu-gcc12  [r-devel-linux-x86_64-debian-gcc]
  29. [CT]  ubuntu-next  [r-next, r-patched, r-patched-linux-x86_64]
  30. [CT]  ubuntu-release  [r-release, r-release-linux-x86_64, ubuntu]
  31. [CT]  valgrind  [valgrind]

## Test coverage
The test coverage [devtools::test_coverage()] of the package is 80%.

## Reverse dependencies
Results from [revdepcheck::revdep_check()]:

```
We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
```

## R CMD check results
There were no ERRORs or WARNINGs. 

### Note 1

```
> checking for future file timestamps ... NOTE
  unable to verify current time
```

According to a discussion on on StackOverflow
(https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time),
this note is caused by the failure to access worldclockapi.com. -> this note is
triggered by the check and not globaltrends

### Note 1
R CMD check might show a notes regarding (possible) invalid URLs. These note do
not signify invalid URLs. All URLs have been manually checked. The same note was
reported in the initial submission.

## winbuilder
There were no ERRORs or WARNINGs.

### Note 1
Winbuilder might show a notes regarding (possible) invalid URLs. These note do
not signify invalid URLs. All URLs have been manually checked. The same note was
reported in the initial submission.

### Note 2
Winbuilder [R version 4.4.3 (2025-02-28 ucrt)] creates the following note:

```
* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Harald Puhr [aut, cre] (ORCID: <https://orcid.org/0000-0002-3308-9553>), Jakob Muellner [ccp] (ORCID: <https://orcid.org/0000-0002-3443-0469>)'
  Authors@R: 'Harald Puhr [aut, cre] (<https://orcid.org/0000-0002-3308-9553>), Jakob Muellner [ccp] (<https://orcid.org/0000-0002-3443-0469>)'
```

However, the author meta-information in the `DESCRIPTION`file is created
automatically:

```
Authors@R: c(
    person("Harald", "Puhr", email = "harald.puhr@gmail.com", role = c("aut", "cre"), comment = c(ORCID = "0000-0002-3308-9553")),
    person("Jakob", "Muellner", role = "ccp", comment = c(ORCID = "0000-0002-3443-0469"))
  )
```

## Summary of changes
In this resubmission of the globaltrends package I have made the following updates:

* Added the function to run downloads from Google Trends through the official
  Google Trends API
* Allow the users to choose to download through the official API or the
  `gtrendsr`package
* Allow controlling the waiting time between queries through an environmental
  variable
* Implemented multiple package simplifications that cut overhead:
  * Remove time-series adjustments from `compute_score` for simplification
  * Remove `get_abnorm_hist` function for package simplification
  * Remove plotting functions and export classes for package simplification
  * Accelerate `compute_score`
  * Accelerate handling of synonyms in separate function `aggregate_synonyms`
* Fixed technical issues and bugs
  * Increase dependencies to `dplyr 1.1.1` and replace the `multiple` argument by
    `relationship`
  * Fixed issues in sample data

Overall, these updates are a significant improvement in the package and consist
a major update to globaltrends.

Thanks!
Harald Puhr

# Resubmission 0.0.14

## Test environments
* local Windows 10 Enterprise, 64 bit (R 4.2.2)
* Windows Server 2022, R-devel, 64 bit on R-Hub
* Ubuntu Linux 20.04.1 LTS, R-release, GCC on R-Hub
* Fedora Linux, R-devel, clang, gfortran on R-Hub
* macOS-latest (release) on GitHub
* windows-latest (release) on GitHub
* ubuntu-latest (devel) on GitHub
* ubuntu-latest (release) on GitHub
* ubuntu-latest (oldrel-1) on GitHub
* Windows Server 2022, R 2023-03-05 r83940 ucrt (devel) on winbuilder
* Windows Server 2022, R 4.2.2 (release) on winbuilder
* Windows Server 2008, R 4.1.3 (oldrelease) on winbuilder

## Test coverage
The test coverage [devtools::test_coverage()] of the package is 96%.

## Reverse dependencies
Results from [revdepcheck::revdep_check()]:

```
We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
```

## R CMD check results
There were no ERRORs or WARNINGs. 

### Note 1
There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this
could be due to a bug/crash in MiKTeX and can likely be ignored. The same note
was reported in the initial submission.

### Note 2
There is one NOTE that is only found on Fedora Linux (R-devel, clang, gfortran):

```
* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
```

As noted in [R-hub issue #548](https://github.com/r-hub/rhub/issues/548), this
issue appears to be independent of the configuration on the local machine.

## winbuilder
Winbuilder might show a note regarding (possible) invalid URLs:

```
URL: https://trends.google.com/trends/explore?q=%2Fm%2F03phgz&geo=AT
  From: inst/doc/globaltrends.html
  Status: 429
  Message: Too Many Requests
```

This does not signify an invalid URL but indicates that Google Trends blocks
downloads due to too many requests.  The same note was reported in the initial
submission.

---

In this resubmission of the globaltrends package I have made the following updates:

* Remove dependency on `glue` package
* Remove dependency on `ineq` package
* Use functions `dbCreateTable` and `dbAppendTable` from `DBI 1.1.0`
* Remove the function `plot_map` from the package and point users to more flexibile alternatives
* Remove dependency on the `maps` package
* Remove reference to non-existent table `data_global` in function `remove_data`
* Adapt test *remove_data6* in `test-computations.R` to avoid check failures on R-oldrease

The adaption of test *remove_data6* in `test-computations.R` will also resolve the
failed CRAN checks for the current version of the package (0.0.13). This is because
the test failure does not signify unintended behavior of the package. Instead it
points to unintended behavior in the test. This issue is now resolved.

Thanks!
Harald Puhr

# Resubmission 0.0.13

## Test environments
* local Windows 10 Enterprise, 64 bit (R 4.2.2)
* Windows Server 2022, R-devel, 64 bit on R-Hub
* Ubuntu Linux 20.04.1 LTS, R-release, GCC on R-Hub
* Fedora Linux, R-devel, clang, gfortran on R-Hub
* macOS-latest (release) on GitHub
* windows-latest (release) on GitHub
* ubuntu-latest (devel) on GitHub
* ubuntu-latest (release) on GitHub
* ubuntu-latest (oldrel-1) on GitHub

## Test coverage
The test coverage [devtools::test_coverage()] of the package is 96%.

## Reverse dependencies
Results from [revdepcheck::revdep_check()]:

```
We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
```

## R CMD check results
There were no ERRORs or WARNINGs. 

### Note 1
There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this
could be due to a bug/crash in MiKTeX and can likely be ignored. The same note
was reported in the initial submission.

### Note 2
There is one NOTE that is only found on Fedora Linux (R-devel, clang, gfortran):

```
* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
```

As noted in [R-hub issue #548](https://github.com/r-hub/rhub/issues/548), this
issue appears to be independent of the configuration on the local machine.

## winbuilder
Winbuilder might show a note regarding (possible) invalid URLs:

```
URL: https://trends.google.com/trends/explore?q=%2Fm%2F03phgz&geo=AT
  From: inst/doc/globaltrends.html
  Status: 429
  Message: Too Many Requests
```

This does not signify an invalid URL but indicates that Google Trends blocks
downloads due to too many requests.  The same note was reported in the initial
submission.

---

In this resubmission of the globaltrends package I have made the following updates:

* Reduced dependencies in line with minimum R version
* Updated vignette
* Changed time intervals for download functions
* Added a function to clean up the SQLite file created by the package
* Adapted functions in line with tidyselect version 1.2.0
* Adapted functions in line with ggplot2 version 3.4.0
* Adapted functions in line with dplyr version 1.1.0
* Facilitated handling of location = "NA - Namibia"
* Dropped dependency on `WDI` package
* Updated URLs in package to avoid notes from CRAN checks

Excluding the dependency on `WDI` from `globaltrends` will also resolve the
failed CRAN checks for the current version of the package (0.0.12), since
these failures relate to failed downloads from the World Bank servers attempted
by the `WDI` package.

Thanks!
Harald Puhr

# Submission 0.0.12

## Test environments
* local Windows 10 Enterprise, 64 bit (R 4.1.3)
* Windows Server 2022, R-devel, 64 bit on R-Hub
* Ubuntu Linux 20.04.1 LTS, R-release, GCC on R-Hub
* Fedora Linux, R-devel, clang, gfortran on R-Hub
* macOS-latest (release) on GitHub
* windows-latest (release) on GitHub
* ubuntu-latest (devel) on GitHub
* ubuntu-latest (release) on GitHub
* ubuntu-latest (oldrel-1) on GitHub

## Test coverage
The test coverage [devtools::test_coverage()] of the package is 96%.

## R CMD check results
There were no ERRORs or WARNINGs. 

The checks result in two NOTEs.

### NOTE 1
There is one NOTE indicating that this is the initial submission of the package to CRAN:

```
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Harald Puhr <harald.puhr@gmail.com>'
  
  New submission
```

### NOTE 2
There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## winbuilder
Winbuilder might show errors regarding (possible) invalid URLs:

```
URL: https://support.google.com/trends/answer/4359550
  From: man/add_keyword.Rd
        inst/doc/globaltrends.html
  Status: 404
  Message: Not Found
```

Unlike indicated by winbuilder, the URL is available and can be accessed in the browser.

```
URL: https://trends.google.com/trends/explore?q=%2Fm%2F03phgz&geo=AT
  From: inst/doc/globaltrends.html
  Status: 429
  Message: Too Many Requests
```

This does not signify an invalid URL but indicates that Google Trends blocks downloads due to too many requests.

---

This is the initial submission of the globaltrends package.


Thanks!
Harald Puhr
