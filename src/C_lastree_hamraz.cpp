#include <Rcpp.h>
#include "Point.h"
#include "HamrazProfiles.h"

using namespace Rcpp;

// [[Rcpp::export]]
List C_hamraz_segmentation(S4 disc, double nps, int sensitivity, double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os, double radius)
{
  // Convert R object to STL object
  DataFrame data = as<Rcpp::DataFrame>(disc.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];
  NumericVector R = data["R"];

  std::vector<PointXYZR*> points(X.size());
  for (int i = 0 ; i < X.size() ; i++)
    points[i] = new PointXYZR(X[i], Y[i], Z[i], i, R[i]);

  int gmx_pos = std::distance(Z.begin(), std::max_element(Z.begin(), Z.end()));
  PointXYZR GMX = *points[gmx_pos];

  // Create the 8 first profiles by contructing a ProfilesManager (page 535 section 2.1)
  Hamraz::ProfilesManager profiles(points, GMX, radius, 2*nps, sensitivity, MDCW, Epsilon, CLc, CLs, Oc, Os);

  // Sequentially add new profiles (page 535 section 2.1)
  while(profiles.chord > 2*nps)
    profiles.add_next_profiles(points);

  Rcpp::List L = profiles.to_R();

  // Free memory
  for (int i = 0 ; i < X.size() ; i++)
    delete points[i];

  return (L);
}