This is an early release to fix troubleshooting on CRAN with 32 bits Solaris: quantization test failure 32 bits case was handled with a _WIN32 preprocesor condition which was not valid for Solaris. The fixed has been tested against rhub and was expected to be integrated in 3.0.1 but I made a mistake this is why the fix was not in 3.0.1.
  
## Test environments

* Linux Mint 19 (Ubuntu 16.04), R 4.0.0 with g++ (7.5.0)
* Linux Mint 19 (Ubuntu 16.04), R 4.0.0 with g++ (8.4.0)
* Linux Mint 19 (Ubuntu 16.04), R 4.0.0 with clang++ (6.0.0)
* Linux Mint 19 (Ubuntu 16.04), R 4.0.0 with clang++ (6.0.0) -std=c++11
* Linux Mint 19 (Ubuntu 16.04), R 4.0.0 with g++ (7.5.0) + valgrind   
* Windows Server 2008 R 4.0.0 via win-builder
* Windows Server 2008 R 3.6.3 via win-builder
* Windows Server 2008 R 3.6.1 via win-builder

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

- checking installed package size ... NOTE

This is compiled code in the libs/ directory.

## Downstream dependencies

We checked 4 reverse dependencies by running R CMD check with this version installed. 
We did not see any new problems.

