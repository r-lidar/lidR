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
  if (!ALTREP(x)) return R_NilValue;
#if R_VERSION >= R_Version(4, 6, 0)
  std::vector<std::string> v;
  auto visitor = [](SEXP name, SEXP attr, void* data) -> SEXP {
    std::vector<std::string>* ptr = static_cast<std::vector<std::string>*>(data);
    std::string s{CHAR(Rf_asChar(attr))};
    ptr->push_back(s);
    return NULL;
  };
  R_mapAttrib(ALTREP_CLASS(x), visitor, static_cast<void*>(&v));
  return Rcpp::wrap(v);
#else
  return ATTRIB(ALTREP_CLASS(x));
#endif
}
