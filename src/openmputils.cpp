#include "myomp.h"

//[[Rcpp::export]]
int R_omp_get_max_threads()
{
  #ifdef _OPENMP
    int max = omp_get_max_threads();
    int lim = omp_get_thread_limit();
    int n = (max < lim) ? max : lim;
    return n;
  #else
    return -1;
  #endif
}
