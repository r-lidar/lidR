## Test environments
* Linux Mint 18 (Ubuntu 16.04), R 3.3.2
* Ubuntu 14.04 (on travis-ci)
* Windows 7 (on VM), R 3.3.2
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  LiDAR (3:17, 12:23)
  las (13:5)
  laz (13:9)
  rasterization (13:20)
  
`LiDAR`: is an acronym for 'light detection and ranging'. No confusion can happen with this well-known term.
`las` and `laz`: are two formats of standardized binary files used to store LiDAR data.
`rasterization` this word exists.

## Downstream dependencies
There are currently no downstream dependencies for this package
