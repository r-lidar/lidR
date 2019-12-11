## Test environments

* Linux Mint 19 (Ubuntu 16.04), R 3.6.1 with g++ (7.4.0)
* Linux Mint 19 (Ubuntu 16.04), R 3.6.1 with g++ (8.3.0)
* Linux Mint 19 (Ubuntu 16.04), R 3.6.1 with clang++ (6.0.0)
* Linux Mint 19 (Ubuntu 16.04), R 3.6.1 with clang++ (6.0.0) -std=c++11
* Linux Mint 19 (Ubuntu 16.04), R 3.6.1 with g++ (7.4.0) + valgrind
* win-builder (oldrel, release, devel)  

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

- checking installed package size ... NOTE

This is compiled code in the libs/ directory + one boost module. The BH package does not embed the 
polygon module so I temporarily included this module. Dirk Eddelbuettel will add this module in the
BH package (see https://github.com/eddelbuettel/bh/issues/63) but he rarely updates. This module
will hopefully be removed with the next version of BH.

## Downstream dependencies

We checked 2 reverse dependencies by running R CMD check with this version installed. 
We did not see any new problems.

