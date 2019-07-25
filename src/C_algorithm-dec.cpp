#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_highest(S4 las, IntegerVector cells, int n)
{
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector Z = data["Z"];
  IntegerVector output(n);
  std::fill(output.begin(), output.end(), -9999999);

  for (int i = 0 ; i < cells.size() ; i++)
  {
    double z = Z[i];
    double j = cells[i]-1;

    if (output[j] == -9999999)
    {
      output[j] = i;
    }
    else
    {
      double zref = Z[output[j]];
      if (zref < z) output[j] = i;
    }
  }

  output = output[output > -9999999];
  return output+1;
}
