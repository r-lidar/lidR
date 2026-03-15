#include <Rcpp.h>

#include "ptd/PTD.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_ptd(DataFrame df, double res, double angle, double distance, double spacing, bool verbose)
{
  NumericVector x = df[0];
  NumericVector y = df[1];
  NumericVector z = df[2];

  // Register parameters
  PTD::Parameters params;
  params.max_iteration_angle = angle;
  params.max_iteration_distance = distance;
  params.spacing = spacing;
  params.seed_resolution_search = res;
  params.verbose = verbose;

  // Register the bounding box of the points
  PTD::Bbox bb;
  bb.xmin = min(x);
  bb.ymin = min(y);
  bb.zmin = min(z);
  bb.xmax = max(x);
  bb.ymax = max(y);
  bb.zmax = max(z);

  // Optional: register a logger
  PTD::Logger logger = [](const std::string& msg) { Rprintf("%s\n", msg.c_str()); };

  PTD::PTD ptd(params, bb);
  ptd.set_logger(logger);

  for (int i = 0 ; i < x.size() ; i++)
  {
    ptd.insert_point(x[i], y[i], z[i], i);
  }

  ptd.run();

  // return the fid of the ground points
  std::vector<unsigned int> gnd = ptd.get_ground_fid();
  return wrap(gnd);
}
