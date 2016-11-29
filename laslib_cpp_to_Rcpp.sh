#!/bin/bash

# The LASlib and LASzip are open source librairies developped by Martin Isenburg to read and write
# las and laz file. I integrated these libraries into lidR to create a wrapper into R to deal
# with las file.

# However R does not like the way LASLib and LASzip are coded. R does not like rand and srand.
# The use of exit(1), and the use of standard output (sdtout, stderr) are not allowed.

# The following regex enable to automatically correct these problems:
# - throw 1 replace exit(1);
# - Rcpp::Rcerr << ... <<< std::end; replace fprintf(strerr, ...);
# - Rcpp.h replace useless headers
# - every words "stdout" even those in commented code (R is very diligent) were replaced by stdoutput
# - srand have been commented
# - R::runif replace rand
# It represents at least 1000 lines automatically modified in 100 files this is why I can't list
# all the modifications

# Some extra changes were done
# lasreaderpipeon.cpp 		  l96 		stdout > Rcpp::Rcout
# laswriter.cpp			  l132-197 	comment lines
# laswriter_las.cpp		  l96-105	comment lines
# laswriter_qfit.cpp		  l82-91	comment lines
# laswriter_bin.cpp		  l126-135	comment lines
# bytestreamount_file.hpp 	  l134		stdout > Rcpp::Rcout

# R is happy now!


files=`find ./ -regex ".*[hc]pp"`

## ====== Remove every stderr, replace by Rcpp::cerr

# replace fprintf stderr without variable ended by \012
perl -pi -w -e 's/fprintf\(stderr,\s*(\"[^%]+)\\n\"\);/Rcpp::Rcerr << $1" << std::endl;/g;' $files

# replace fprintf stderr without variable ended by \n
perl -pi -w -e 's/fprintf\(stderr,\s*(\"[^%]+)\\n\"\);/Rcpp::Rcerr << $1" << std::endl;/g;' $files

# replace fprintf stderr with 1 variable ended by \012
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\\012\"),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $5 << "$3" << std::endl;/g;' $files

# replace fprintf stderr with 1 variable ended by \n
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\\n\"),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $5 << "$3" << std::endl;/g;' $files

# replace fprintf stderr with 2 variables
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\\n\"),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $7 << "$3" << $8 << "$5" << std::endl;/g;' $files

# replace fprintf stderr with 3 variables
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\\n\"),\s*([^,]*),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $9 << "$3" << $10 << "$5" << $11 << "$7" << std::endl;/g;' $files

# replace fprintf stderr with 4 variables
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\\n\"),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $11 << "$3" << $12 << "$5" << $13 << "$7" << $14 << "$9" << std::endl;/g;' $files

# replace fprintf stderr with 5 variables ended by \n
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\%[dI64ldsgcu]*)(.*)(\\n\"),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $13 << "$3" << $14 << "$5" << $15 << "$7" << $16 << "$9" << $17 << "$11" << std::endl;/g;' $files

# special replacement cpp laslib
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%g)\s(\%g)\s(\%g)\svs\s(\%g)\s(\%g)\s(\%g)(.*)(\\n\"),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $10 << " " << $11 << " " << $12 << " " << $13 << " " << $14 << " " << $15 << std::endl;/g;' $files

# special replacement hpp laslib
perl -pi -w -e 's/fprintf\(stderr,\s*(\".*)(\%g)\s(\%g)\s(\%g)\s\/\s(\%g)\s(\%g)\s(\%g)(.*)(\\n\"),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*),\s*([^,]*)\);/Rcpp::Rcerr << $1" << $10 << " " << $11 << " " << $12 << " " << $13 << " " << $14 << " " << $15 << "$8" << std::endl;/g;' $files

# Remove string and stdlib replaced by Rcpp
perl -pi -w -e 's/#include <string.h>/#include <Rcpp.h>/g;' $files
perl -pi -w -e 's/#include <stdio.h>/#include <stdio.h>\n#include <Rcpp.h>/g;' $files
perl -pi -w -e 's/#include <stdlib.h>//g;' $files

## ====== Remove every exit(1), replace by throw 1

perl -pi -w -e 's/exit\(1\)/throw 1/g;' $files

## ====== Remove every stdout, replace by Rcpp::Rcout

perl -pi -w -e 's/fileno\(stdout/fileno\(Rcpp::Rcout/g;' $files
perl -pi -w -e 's/stdout/stdoutput/g;' $files

## ====== Remove srand and rand, replace rand by R::runif(0, RAND_MAX)

perl -pi -w -e 's/srand\(seed\)/\/\/srand\(seed\)/g;' $files
perl -pi -w -e 's/rand\(\)/R::runif\(0, RAND_MAX\)/g;' $files

