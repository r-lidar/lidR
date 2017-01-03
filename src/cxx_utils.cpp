#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector fast_table(IntegerVector x, int size = 5)
{
  IntegerVector tbl(size);

  for (IntegerVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it <= size)
      tbl(*it-1)++;
  }

  return tbl;
}
