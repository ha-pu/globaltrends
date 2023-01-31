# Resubmission

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
The test coverage [devtools:::test_coverage()] of the package is 96%.

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

## winbulider
Winbuilder might show errors regarding (possible) invalid URLs:

```
URL: https://support.google.com/trends/answer/4359550
  From: man/add_keyword.Rd
        inst/doc/globaltrends.html
  Status: 404
  Message: Not Found
```

Unlike indicated by winbuilder, the URL is available and can be accessed in the
browser. The same note was reported in the initial submission.

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
* Facilitated handling of location = "NA - Namibia"
* Dropped dependency on `WDI` package

Thanks!
Harald Puhr

# Submission

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
The test coverage [devtools:::test_coverage()] of the package is 96%.

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

## winbulider
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
