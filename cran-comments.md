This is an early release to fix troubleshooting on CRAN. The release of v3.0.0 came with 4 problems:

- ASAN/USBAN: fixed. I volontarily introduced a test with invalid data that can only happen with corrupted files and did not thougth about the consequences. I removed the test.
- Quantization test failure on 32 bit Solaris: fixed. 32 bits case was handled with a _WIN32 preprocesor condition which was not valid for Solaris. The fixed has been tested against rhub
- st_crs issue on Solaris. Fixed with a workaround hack. The problem was not related to my code but to the current version of gdal and proj on Solaris which seem is not the same than on each flavours (I guess). I was able to reproduce on TravisCI only. It is fixed on this platform so I guess it is fixed on other ones
- Test failure on Fedora and MacOS. Fixed.
  
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

