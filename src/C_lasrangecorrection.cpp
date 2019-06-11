#include <Rcpp.h>
#include "Progress.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_lasrangecorrection(S4 las, DataFrame flightlines, double Rs, double f)
{
  DataFrame data  = as<DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];
  NumericVector T = data["gpstime"];
  IntegerVector I = data["Intensity"];

  NumericVector x = flightlines["X"];
  NumericVector y = flightlines["Y"];
  NumericVector z = flightlines["Z"];
  NumericVector t = flightlines["gpstime"];

  NumericVector::iterator it;
  double dx, dy, dz, dt, r, R, k, range;
  double i;
  int j;

  IntegerVector Inorm(X.size());

  Progress pbar(X.size(), "Range computation");

  for (int k = 0 ; k < X.size() ; k++)
  {
    pbar.increment();
    pbar.check_abort();

    it = std::lower_bound(t.begin(), t.end(), T[k]);

    // If the gpstime is the last one: no interpolation with the next one (edge of data)
    if (it == t.end())
    {
      dx = X[k] - x[j];
      dy = Y[k] - y[j];
      dz = Z[k] - z[j];
    }
    // If t2-t1 is too big it is two differents lines: no interpolation with the next one (edge of data)
    else if (*(it+1) - *it > 30)
    {
      dx = X[k] - x[j];
      dy = Y[k] - y[j];
      dz = Z[k] - z[j];
    }
    // General case with t2 > t > t1
    else
    {
      j  = it - t.begin();
      r  = 1 - (t[j+1]-T[k])/(t[j+1]-t[j]);

      dx = X[k] - (x[j] + (x[j+1] - x[j])*r);
      dy = Y[k] - (y[j] + (y[j+1] - y[j])*r);
      dz = Z[k] - (z[j] + (z[j+1] - z[j])*r);
    }

    R = std::sqrt(dx*dx + dy*dy + dz*dz);

    i = I[k] * std::pow((R/Rs),f);

    if (i > 65535)
      i = 65535;

    Inorm[k]  = i;
  }

  return Inorm;
}
