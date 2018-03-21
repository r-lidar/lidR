// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include <limits>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_lassmooth(S4 las, double size)
{
  // DataFrame data = las.slot("data");
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  long n = X.length();
  double half_res = size / 2;

  NumericVector Z_temp;
  NumericVector Z_out  = clone(Z);

  QuadTree *tree = QuadTreeCreate(X,Y);

  Progress p(n, false);

  // Dilate
  for (long i = 0 ; i < n ; i++)
  {
    if (Progress::check_abort() )
      return Z_out;
    else
      p.update(i);

    std::vector<Point*> pts;
    tree->rect_lookup(X[i], Y[i], half_res, half_res, pts);

    double zmoy = 0;
    for(long j = 0 ; j < pts.size() ; j++)
      zmoy += Z[pts[j]->id];

    Z_out[i] = zmoy/pts.size();
  }

  delete tree;

  return Z_out;
}