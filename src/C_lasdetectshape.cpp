/*
 ===============================================================================

 PROGRAMMERS:

jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

COPYRIGHT:

Copyright 2019 Jean-Romain Roussel

This file is part of lidR R package.

lidR is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>

===============================================================================
*/

#include <RcppArmadillo.h>
#include "Progress.h"
#include "QuadTree.h"
#include "myomp.h"

using namespace Rcpp;

bool coplanar (arma::vec& latent, arma::mat& coeff, NumericVector& th) { return latent[1] > th[0]*latent[2] && th[1]*latent[1] > latent[0]; }
bool hcoplanar(arma::vec& latent, arma::mat& coeff, NumericVector& th) { return latent[1] > th[0]*latent[2] && th[1]*latent[1] > latent[0] && std::abs(coeff(2,2)) > th[2]; }
bool colinear (arma::vec& latent, arma::mat& coeff, NumericVector& th) { return th[0]*latent[2] < latent[0] && th[0]*latent[1] < latent[0]; }

// [[Rcpp::export]]
LogicalVector C_lasdetectshape(S4 las, int method, NumericVector th, int k, LogicalVector filter, int ncpu)
{
  DataFrame data  = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  unsigned int n = X.length();

  LogicalVector output(n);

  Progress pb(n, "Eigenvalues computation: ");

  bool abort = false;
  bool use_filter = filter.size() == n;

  std::vector<bool> f(n);
  std::fill(f.begin(), f.end(), true);
  if (use_filter) f = as< std::vector<bool> >(filter);

  QuadTree qtree(las, f);

  bool (*predicate)(arma::vec&, arma::mat&, NumericVector&);
  switch(method)
  {
    case 1: predicate = &coplanar; break;
    case 2: predicate = &hcoplanar; break;
    case 3: predicate = &colinear; break;
  }

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < n ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true; // No data race here because only thread 0 can actually write
    pb.increment();
    if (use_filter && !f[i]) continue;

    arma::mat A(k,3);
    arma::mat coeff;  // Principle component matrix
    arma::mat score;
    arma::vec latent; // Eigenvalues in descending order

    PointXYZ p(X[i], Y[i], Z[i]);

    std::vector<PointXYZ> pts;
    qtree.knn(p, k, pts);

    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      A(j,0) = pts[j].x;
      A(j,1) = pts[j].y;
      A(j,2) = pts[j].z;
    }

    arma::princomp(coeff, score, latent, A);

    #pragma omp critical
    {
      output[i] = predicate(latent, coeff, th);
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return(output);
}
