/* This functions exist for units test only */

#include <Rcpp.h>
#include "QuadTree.h"
#include "QuadTree3D.h"
#include "Progress.h"

using namespace Rcpp;

typedef std::vector<PointXYZ> vpoint;
typedef QuadTree3D<PointXYZ> QuadTree3;

// [[Rcpp::export]]
IntegerVector C_circle_lookup(NumericVector X, NumericVector Y, double x, double y, double r)
{
  std::vector<int> id;

  QuadTree *tree = QuadTreeCreate(X,Y);
  std::vector<Point*> pts;
  tree->circle_lookup(x, y, r, pts);

  for (size_t j =0 ; j < pts.size() ; j++)
    id.push_back(pts[j]->id + 1);

  delete tree;
  return wrap(id);
}

// [[Rcpp::export]]
IntegerVector C_knn3d_lookup(NumericVector X, NumericVector Y, NumericVector Z, double x, double y, double z, int k)
{
  std::vector<int> id;
  unsigned int n = X.size();

  vpoint points(n);
  for (size_t i = 0 ; i < n ; i++)
    points[i] = PointXYZ(X[i], Y[i], Z[i], i);

  // Creation of a QuadTree
  QuadTree3* tree = QuadTreeCreate(points);

  PointXYZ p(x,y,z);
  std::vector<PointXYZ> pts;
  tree->knn_lookup3D(p, k, pts);

  for (size_t j =0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  delete tree;
  return wrap(id);
}


