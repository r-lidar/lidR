This is an early release to fix the additionnal issues with ASAN USBAN and valgrind caused by unitialized values.

## Test environments

* Linux Mint 19 (Ubuntu 16.04), R 3.6.0 with gcc
* Linux Mint 19 (Ubuntu 16.04), R 3.6.0 with clang
* win-builder (oldrel, release, devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

- checking installed package size ... NOTE

This is all compiled code in the libs/ directory.

## Downstream dependencies

We checked 2 reverse dependencies by running R CMD check with this version installed. 
We did not see any new problems. However for TreeLS one example (at least) of the documentation
should raise a warning because one of a function that is now less tolerant with invalid data given
as input. The developper is aware of this fact.

