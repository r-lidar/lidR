#include "myomp.h"

//[[Rcpp::export]]
int R_omp_get_max_threads()
{
  #ifdef _OPENMP
    int n = omp_get_max_threads();
    return n;
  #else
    return -1;
  #endif
}
