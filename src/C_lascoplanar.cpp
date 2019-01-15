/*
 ===============================================================================

 PROGRAMMERS:

 jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

 COPYRIGHT:

 Copyright 2018 Jean-Romain Roussel

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

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector C_lascoplanar(S4 las, int k, double th1, double th2, LogicalVector filter)
{
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  unsigned int n = X.length();

  LogicalVector output(n);

  arma::mat A(k,3);
  arma::mat coeff;
  arma::mat score;
  arma::vec latent;

  Progress pb(n, "Eigenvalues computation: ");

  bool colinear_mode = (th2 == 0) ? true : false;
  bool use_filter = filter.size() == n;

  std::vector<bool> f(n);
  std::fill(f.begin(), f.end(), true);
  if (use_filter) f = as< std::vector<bool> >(filter);

  QuadTree qtree(las, f);

  for (unsigned int i = 0 ; i < n ; i++)
  {
    pb.check_abort();
    pb.increment();

    if (use_filter && !f[i]) continue;

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

    if (!colinear_mode && latent[1] > th1*latent[2] && th2*latent[1] > latent[0])
      output[i] = true;
    else if (colinear_mode && th1*latent[2] < latent[0] && th1*latent[1] < latent[0])
      output[i] = true;
  }

  return(output);
}
