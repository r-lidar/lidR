#include <Rcpp.h>
#include "myomp.h"

using namespace Rcpp;

LogicalVector C_lmf(DataFrame, NumericVector, double, bool, int);
