## Resubmission v0.0.1.5
* modified url's in readme and description

## Resubmission v0.0.1.5
* Fixed dependency issue. Dependent package fishualize was archived but is now back on cran

## Resubmission v0.0.1.4
This is a resubmission. In this version I have:

* Adapted a table in the package vignette so that pandoc is not nescessary
* Fixed concerns with the package vignette -> I now recommend installation through cran.
* Changed package dependency with rfishbase, which solved the API error


## Resubmission v0.0.1.4
This is a resubmission. In this version I have:

* Fixed issues with the package vignette
* I was asked to add \value to each function, which I did but with \returns 
* I added dontrun{} around all examples relying on the r package rfishbase, just as the developers of that package did. 

## Resubmission v0.0.1.2
This is a resubmission. In this version I have:

* Fixed issues in test files to comply with the following CRAN policy:
'Packages which use Internet resources should fail gracefully with an
informative message if the resource is not available or has changed (and
not give a check warning nor error).'

* Fixed url in readme 

## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
