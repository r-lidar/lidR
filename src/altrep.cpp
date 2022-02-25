#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool is_altrep(SEXP x)
{
  return ALTREP(x);
}

// [[Rcpp::export]]
bool is_materialized(SEXP x)
{
  return DATAPTR_OR_NULL(x) != nullptr;
}

// [[Rcpp::export]]
SEXP altrep_full_class(SEXP x) {
  if (ALTREP(x)) return ATTRIB(ALTREP_CLASS(x));
  return R_NilValue;
}

