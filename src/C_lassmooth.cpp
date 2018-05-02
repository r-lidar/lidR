// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include <limits>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_lassmooth(S4 las, double size, int method = 1, int shape = 1, double sigma = 1)
{
  // shape: 1- rectangle 2- circle
  // method: 1- average 2- gaussian
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  long n = X.length();
  double half_res = size / 2;
  double twosquaresigma = 2*sigma*sigma;
  double twosquaresigmapi = twosquaresigma * PI;

  NumericVector Z_temp;
  NumericVector Z_out  = clone(Z);

  QuadTree *tree = QuadTreeCreate(X,Y);

  Progress p(n, false);

  for (long i = 0 ; i < n ; i++)
  {
    if (Progress::check_abort())
      return Z_out;
    else
      p.update(i);

    std::vector<Point*> pts;

    if(shape == 1)
      tree->rect_lookup(X[i], Y[i], half_res, half_res, pts);
    else
      tree->circle_lookup(X[i], Y[i], half_res, pts);

    double w = 0;
    double ztot = 0;
    double wtot = 0;

    for(long j = 0 ; j < pts.size() ; j++)
    {
      if (method == 1)
      {
        w = 1;
      }
      else
      {
        double dx =  X[i] - pts[j]->x;
        double dy =  Y[i] - pts[j]->y;
        w = 1/twosquaresigmapi * std::exp(-(dx*dx + dy*dy)/twosquaresigma);
      }

      ztot += w*Z[pts[j]->id];
      wtot += w;
    }

    Z_out[i] = ztot/wtot;
  }

  delete tree;
  return Z_out;
}