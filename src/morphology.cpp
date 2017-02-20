#include <Rcpp.h>
#include <limits>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector MorphologicalOpening(NumericVector X, NumericVector Y, NumericVector Z, double resolution)
{
  int n = X.length();
  double half_res = resolution / 2;

  NumericVector Z_temp = clone(Z);
  NumericVector Z_out  = clone(Z);

  QuadTree tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  // Dilate
  for (int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    tree.rect_lookup(X[i], Y[i], half_res, pts);

    double min_pt(std::numeric_limits<double>::max());

    for(int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j]->id];

      if(z < min_pt)
        min_pt = z;
    }

    Z_out[i] = min_pt;
  }

  Z_temp = clone(Z_out);

  // erode
  for (int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    tree.rect_lookup(X[i], Y[i], half_res, pts);

    double max_pt(std::numeric_limits<double>::min());

    for(int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j]->id];

      if(z > max_pt)
        max_pt = z;
    }

    Z_out[i] = max_pt;
  }

  return Z_out;
}