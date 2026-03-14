#include <Rcpp.h>

#include "spikefree/Spikefree.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_spikefree(DataFrame df, DataFrame grid, double d_f, double h_b)
{
  NumericVector x = df[0];
  NumericVector y = df[1];
  NumericVector z = df[2];

  NumericVector X = grid[0];
  NumericVector Y = grid[1];
  NumericVector Z(X.size());

  // Register parameters
  Spikefree::Parameters params;
  params.d_f = d_f;
  params.h_b = h_b;

  // Register the bounding box of the points
  Spikefree::Bbox bb;
  bb.xmin = min(x);
  bb.ymin = min(y);
  bb.zmin = min(x);
  bb.xmax = max(x);
  bb.ymax = max(y);
  bb.zmax = max(z);

  // Optional: register a logger
  Spikefree::Logger logger = [](const std::string& msg) { Rprintf("%s\n", msg.c_str()); };

  // Get the order in which to process points (Z decreasing)
  std::vector<int> idx(x.size());
  std::iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(), [&](int i, int j) { return z[i] > z[j]; });

  Spikefree::Spikefree sf(params, bb);
  sf.set_logger(logger);

  for (int i = 0 ; i < x.size() ; i++) sf.pre_insert_point(x[i], y[i], z[i]);
  for (int i : idx) sf.insert_point(x[i], y[i], z[i]);

  #pragma omp parallel for
  for (int i = 0; i < X.size(); ++i)
  {
    double cx = X[i];
    double cy = Y[i];
    double cz = sf.get_z(cx,cy);
    if (std::isnan(cz)) cz = NA_REAL;
    Z[i] = cz;
  }

  return Z;
}

