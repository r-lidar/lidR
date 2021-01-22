Current status on CRAN

- clang-UBSAN: 
  - Error: outside the range of representable values of type 'int'
    Fix  : this error was trivial and fixed.
  - Error: outside the range of representable values of type 'int'
    Fix  : This one is different than the previous one but was trivial and fixed
- gcc-ASAN:
  - Error: new-delete-type-mismatch on 0x60300006d7b0 in thread T0:
    Fix  : This one is a false positive from the `rlas` package. I reproduced and I believed I fixed it 
    in the previous release. It seems not and `rlas` does not detect it. I removed some files 
    in extdata that are responsible of such error. The error should no longer be triggered
- Solaris & Mac OS
  - Error: no arguments in initialization list
    Fix  : This one is from the `sp` package. I previously updated my code to use the new 
    syntax of `sp`but this seems to fail on solaris and MacOS. I don't know why. Thus I 
    reverted to the old syntax that formerly worked.
- oldrel-windows
   - Error: Installation failed
     Fix  : The latest release of BH is based on Boost 1.75 and requires to compile in C++14.
     Consequently we (several packages affected) moved to C++14 without trouble except on
     oldrel-windows. lidR, rlas, googlePolylines, wellknown, are equally affected and we can't
     go back to C++11. Since oldrel is expected to be the current release in few days I guess
     it is ok and this will be resolved automatically soon.
     
## Test environments

* Linux Mint 20 (Ubuntu 20.04), R 4.0.3 with g++
* Linux Mint 20 (Ubuntu 20.04), R 4.0.3 with g++) + valgrind 
* Windows Server 2009 via github CI
* Ubuntu 20.04 R-release via github CI
* Ubuntu 20.04 R-devel via github CI
* Windows Server 2008 R-release via win-builder
* Windows Server 2008 Rdevel via win-builder

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

- checking installed package size ... NOTE

This is compiled code in the libs/ directory.

## Downstream dependencies

We checked 4 reverse dependencies by running R CMD check with this version installed. 
We did not see any new problems.

