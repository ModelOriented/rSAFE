## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
* Fixed  arXiv id i n the DESCRIPTION.
* Reduced timing for tests: xgboost removed from tests, reduced number of rows in data.
* Removed "in R" from the package title.
* Removed set.seed() from the functions.
* Added value to print.safe_extractor method.
