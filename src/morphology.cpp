#include <Rcpp.h>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector MorphologicalOpening(DataFrame cloud_in, double resolution)
{
  NumericVector X = cloud_in["X"];
  NumericVector Y = cloud_in["Y"];
  NumericVector Z = cloud_in["Z"];

  NumericVector Z_temp = clone(Z);
  NumericVector Z_out  = clone(Z);

  int n = X.length();
  double half_res = resolution / 2;

  double xmin = min(X);
  double xmax = max(X);
  double ymin = min(Y);
  double ymax = max(Y);
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double range = xrange > yrange ? xrange/2 : yrange/2;

  QuadTree tree( (xmin+xmax)/2, (ymin+ymax)/2, range);

  for(int i = 0 ; i < n ; i++)
	{
		Point p(X[i], Y[i], i);
		tree.insert(p);
  }

  // Dilate
  for (int i = 0 ; i < n ; i++)
  {
    Point center(X[i], Y[i]);
    Point hlfdim(half_res, half_res);

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
    Point center(X[i], Y[i]);
    Point hlfdim(half_res, half_res);

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