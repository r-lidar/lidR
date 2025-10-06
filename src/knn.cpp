#include "nanoflann.hpp"
#include "Progress.h"

#include <Rcpp.h>
#include "myomp.h"

using namespace Rcpp;

class DataFrameAdaptor
{
public:
  std::vector<Rcpp::NumericVector> coords;
  size_t dim;

  DataFrameAdaptor(const Rcpp::DataFrame& df, std::vector<std::string> col_names)
  {
    dim = col_names.size();
    coords.reserve(dim);
    for (const auto& name : col_names)
      coords.push_back(df[name]);
  }

  inline size_t kdtree_get_point_count() const { return coords[0].size(); }
  inline double kdtree_get_pt(const size_t idx, const size_t d) const {
    return coords[d][idx];
  }
  template <class BBOX> bool kdtree_get_bbox(BBOX&) const { return false; }
};

using KDTree = nanoflann::KDTreeSingleIndexAdaptor<nanoflann::L2_Simple_Adaptor<double, DataFrameAdaptor>, DataFrameAdaptor, 3>;

// [[Rcpp::export]]
List cpp_knn(S4 data, int k, int ncpu)
{

  DataFrame df = as<DataFrame>(data.slot("data"));
  NumericVector X = df["X"];
  NumericVector Y = df["Y"];
  NumericVector Z = df["Z"];

  DataFrameAdaptor adaptor(df, {"X", "Y", "Z"});
  KDTree tree = KDTree(3, adaptor, nanoflann::KDTreeSingleIndexAdaptorParams(10));
  tree.buildIndex();

  int npoints = X.size();
  k = std::min(k, npoints);

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

    std::vector<uint32_t> indices(k);
    std::vector<KDTree::DistanceType> dists(k);
    double p[3] = { X[i], Y[i], Z[i] };
    tree.knnSearch(p, k, indices.data(), dists.data());

    for (auto j = 1; j < indices.size(); j++)
    {
      knn_idx(i,j-1) = indices[j]+1;
      knn_dist(i,j-1) = std::sqrt(dists[j]);
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return List::create(Named("nn.index") = knn_idx, Named("nn.dist") = knn_dist);
}

// [[Rcpp::export]]
List cpp_knnx(S4 data, S4 query, int k, int ncpu)
{
  DataFrame temp = as<DataFrame>(data.slot("data"));
  DataFrameAdaptor adaptor(temp, {"X", "Y", "Z"});
  KDTree tree = KDTree(3, adaptor, nanoflann::KDTreeSingleIndexAdaptorParams(10));
  tree.buildIndex();

  DataFrame tmp = as<DataFrame>(query.slot("data"));
  NumericVector X = tmp["X"];
  NumericVector Y = tmp["Y"];
  NumericVector Z = tmp["Z"];

  int npoints = X.size();
  k = std::min(k, temp.nrow());

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

    std::vector<uint32_t> indices(k);
    std::vector<KDTree::DistanceType> dists(k);
    double p[3] = { X[i], Y[i], Z[i] };
    tree.knnSearch(p, k, indices.data(), dists.data());

    for (auto j = 0; j < indices.size(); j++)
    {
      knn_idx(i,j) = indices[j]+1;
      knn_dist(i,j) = std::sqrt(dists[j]);
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return List::create(Named("nn.index") = knn_idx, Named("nn.dist") = knn_dist);
}
