# This script generates a configure file
# Simply run `autoconf` from the project root directory

AC_INIT
AC_LANG(C++)

# Check for c++17 availability
CXX17=`"${R_HOME}/bin/R" CMD config CXX17`
if test -z "$CXX17"; then
  AC_MSG_ERROR([No C++17 compiler is available])
fi

# This seems to be the preferred way to get compiler arguments. Not sure what to do with these.
# c.f. https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#C-standards and https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Using-C_002b_002b-code
# CXX17STD=`"${R_HOME}/bin/R" CMD config CXX17STD`
# CXX="${CXX17} ${CXX17STD}"
# CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX17FLAGS`

# Specify which args to substitute
CXX_STD=CXX17
AC_SUBST(CXX_STD)
AC_CONFIG_FILES([src/Makevars src/Makevars.win])
AC_OUTPUT
