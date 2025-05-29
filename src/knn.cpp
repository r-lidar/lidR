#include "SpatialIndex.h"
#include "Progress.h"

#include <Rcpp.h>
#include "myomp.h"

using namespace Rcpp;
using namespace lidR;

// [[Rcpp::export]]
List cpp_knn(S4 data, int k, int ncpu)
{
  SpatialIndex tree(data);

  DataFrame tmp = as<DataFrame>(data.slot("data"));
  NumericVector X = tmp["X"];
  NumericVector Y = tmp["Y"];
  NumericVector Z = tmp["Z"];

  int npoints = X.size();

  IntegerMatrix knn_idx(npoints, k);
  NumericMatrix knn_dist(npoints, k);

  k++;

  // Initialize the progress bar
  Progress pb(npoints, "knn");

  bool abort = false;

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0; i < npoints; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    std::vector<PointXYZ> pts;
    PointXYZ p(X[i], Y[i], Z[i]);
    tree.knn(p, k, pts);

    for (unsigned int j = 1; j < pts.size(); j++)
    {
      double dx = X[i] - pts[j].x;
      double dy = Y[i] - pts[j].y;
      double dz = Z[i] - pts[j].z;
      double d = std::sqrt(dx*dx+dy*dy+dz*dz);

      knn_idx(i,j-1) = pts[j].id+1;
      knn_dist(i,j-1) = d;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return List::create(Named("nn.index") = knn_idx, Named("nn.dist") = knn_dist);
}

// [[Rcpp::export]]
List cpp_knnx(S4 data, S4 query, int k, int ncpu)
{
  SpatialIndex tree(data);

  DataFrame tmp = as<DataFrame>(query.slot("data"));
  NumericVector X = tmp["X"];
  NumericVector Y = tmp["Y"];
  NumericVector Z = tmp["Z"];

  int npoints = X.size();

  IntegerMatrix knn_idx(npoints, k);
  NumericMatrix knn_dist(npoints, k);

  // Initialize the progress bar
  Progress pb(npoints, "knn");

  bool abort = false;

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    std::vector<PointXYZ> pts;
    PointXYZ p(X[i], Y[i], Z[i]);
    tree.knn(p, k, pts);

    for (unsigned int j = 0 ; j < pts.size(); j++)
    {
      double dx = X[i] - pts[j].x;
      double dy = Y[i] - pts[j].y;
      double dz = Z[i] - pts[j].z;
      double d = std::sqrt(dx*dx+dy*dy+dz*dz);

      knn_idx(i,j) = pts[j].id+1;
      knn_dist(i,j) = d;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return List::create(Named("nn.index") = knn_idx, Named("nn.dist") = knn_dist);
}
