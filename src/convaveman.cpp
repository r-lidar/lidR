#include "Rcpp.h"
#include "concaveman/concaveman.h"

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame cpp_concaveman(NumericVector x, NumericVector y, double concavity, double lengthThreshold, IntegerVector chull)
{
  typedef std::array<double, 2> point_type;

  int num_points = x.size();

  std::vector<point_type> points(num_points);
  for (auto i = 0; i < num_points; i++)
    points[i] = {x[i], y[i]};

  std::vector<int> hull = as< std::vector<int> >(chull);

  auto concave_points = concaveman<double, 16>(points, hull, concavity, lengthThreshold);

  unsigned int n = concave_points.size();
  NumericVector xhull(n + 1);
  NumericVector yhull(n + 1);

  for (unsigned int i = 0; i < n; i++)
  {
    xhull[i] = concave_points[i][0];
    yhull[i] = concave_points[i][1];
  }

  xhull[n] = xhull[0];
  yhull[n] = yhull[0];

  return DataFrame::create(Named("x") = xhull, Named("y") = yhull);
}
