## Test environments
* Linux Mint 18 (Ubuntu 16.04), R 3.3.2
* Ubuntu 14.04 (on travis-ci)
* Windows 7 (on VM), R 3.3.2
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

### On GNU/Linux
There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jean-Romain Roussel <jean-romain.roussel.1@ulaval.ca>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  LiDAR (3:17, 10:23)
  las (11:5)
  laz (11:9)
  rasterization (11:20)

`LiDAR`: is an acronym for 'light detection and ranging'. No confusion can happen with this well-known term.
`las` and `laz`: are two formats of standardized binary files used to store LiDAR data.
`rasterization`: this word exists.

* checking installed package size ... NOTE
  installed size is  6.5Mb
  sub-directories of 1Mb or more:
    libs   5.5Mb

The source code of the `LASlib` library is included and compiled into the package.
`LASlib` is not packaged for Linux and contains deprecated code. 
I modified the code to be compatible both with R and C++98 standards.

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

GNU make serves only to use the wildcard both in makevar and makevar.win 
Nobody has reported an issue relative to GNU make on Windows.

### On Windows
There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jean-Romain Roussel <jean-romain.roussel.1@ulaval.ca>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  LiDAR (3:17, 10:23)
  las (11:5)
  laz (11:9)
  rasterization (11:20)

See GNU/Linux section

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

See GNU/Linux section

## Downstream dependencies
There are currently no downstream dependencies for this package
