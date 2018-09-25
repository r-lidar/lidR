/* These functions exist for unit tests only */

#include <Rcpp.h>
#include "QuadTree.h"
#include "Progress.h"

using namespace Rcpp;
typedef std::vector<PointXYZ> vpoint;

// [[Rcpp::export]]
IntegerVector C_circle_lookup(NumericVector X, NumericVector Y, double x, double y, double r)
{
  std::vector<int> id;

  QuadTree tree(X,Y);
  std::vector<Point*> pts;
  Circle circ(x,y,r);
  tree.lookup(circ, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j]->id + 1);

  return wrap(id);
}

// [[Rcpp::export]]
IntegerVector C_knn3d_lookup(NumericVector X, NumericVector Y, NumericVector Z, double x, double y, double z, int k)
{
  std::vector<int> id;

  // Creation of a QuadTree
  QuadTree tree(X, Y, Z);

  PointXYZ p(x,y,z);
  std::vector<PointXYZ> pts;
  tree.knn(p, k, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  return wrap(id);
}


