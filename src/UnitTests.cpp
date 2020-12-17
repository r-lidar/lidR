#include <Rcpp.h>
#include "SpatialIndex.h"
using namespace Rcpp;
using namespace lidR;

// [[Rcpp::export(rng = false)]]
IntegerVector C_circle_lookup(S4 las, double x, double y, double r)
{
  std::vector<int> id;

  SpatialIndex tree(las);
  std::vector<PointXYZ> pts;
  Circle circ(x,y,r);
  tree.lookup(circ, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  return wrap(id);
}

// [[Rcpp::export(rng = false)]]
IntegerVector C_orectangle_lookup(S4 las, double x, double y, double w, double h, double angle)
{
  std::vector<int> id;

  double xmax = x+w/2;
  double ymax = y+h/2;
  double xmin = x-w/2;
  double ymin = y-h/2;

  SpatialIndex tree(las);
  std::vector<PointXYZ> pts;
  OrientedRectangle orect(xmin, xmax, ymin, ymax, angle);
  tree.lookup(orect, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  return wrap(id);
}

// [[Rcpp::export(rng = false)]]
IntegerVector C_knn2d_lookup(S4 las, double x, double y, int k)
{
  std::vector<int> id;

  SpatialIndex tree(las);

  PointXY p(x,y);
  std::vector<PointXYZ> pts;
  tree.knn(p, k, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  return wrap(id);
}


// [[Rcpp::export(rng = false)]]
IntegerVector C_knn3d_lookup(S4 las, double x, double y, double z, int k)
{
  std::vector<int> id;

  SpatialIndex tree(las);

  PointXYZ p(x,y,z);
  std::vector<PointXYZ> pts;
  tree.knn(p, k, pts);

  for (size_t j = 0 ; j < pts.size() ; j++)
    id.push_back(pts[j].id + 1);

  return wrap(id);
}


