This release comes after prof Bryan Ripley's request: "We see occasional check failures, e.g. with clang 
and sanitizers". Indeed some test datasets were randomly generated and a tolerance on the output was added 
to deal with the variability of the output. Variability can be greater than expected in rare cases.
I added fixed and hard coded seeds, thus the tests are not subject to variability. I remove empirically
etablished tolerances. This issue will not happen anymore.

## Test environments
* Linux Mint 19 (Ubuntu 16.04), R 3.4.4
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package
