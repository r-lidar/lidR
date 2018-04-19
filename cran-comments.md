This is an early release to fix issues on fedora and macos. New policies regarding bioconductor packages
result in errors with one example of the doc and one unit test. I removed the incriminated example and 
the test which implie the bioconductor package. The package is still only a suggested dependency. 

## Test environments
* Linux Mint 19 (Ubuntu 16.04), R 3.4.3
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package
