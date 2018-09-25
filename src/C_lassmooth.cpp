/*
 ===============================================================================

 PROGRAMMERS:

 jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

 COPYRIGHT:

 Copyright 2016-2018 Jean-Romain Roussel

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

#include <Rcpp.h>
#include <limits>
#include "QuadTree.h"
#include "Progress.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_lassmooth(S4 las, double size, int method = 1, int shape = 1, double sigma = 1)
{
  // shape: 1- rectangle 2- circle
  // method: 1- average 2- gaussian
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  unsigned int n = X.length();
  double half_res = size / 2;
  double twosquaresigma = 2*sigma*sigma;
  double twosquaresigmapi = twosquaresigma * M_PI;

  NumericVector Z_temp;
  NumericVector Z_out  = clone(Z);

  QuadTree tree(X,Y);

  Progress p(n, "Point cloud smoothing: ");

  for (unsigned int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;

    if(shape == 1)
    {
      Rectangle rect(X[i]-half_res, X[i]+half_res, Y[i]-half_res,  Y[i]+half_res);
      tree.lookup(rect, pts);
    }
    else
    {
      Circle circ(X[i], Y[i], half_res);
      tree.lookup(circ, pts);
    }

    double w = 0;
    double ztot = 0;
    double wtot = 0;

    for(unsigned int j = 0 ; j < pts.size() ; j++)
    {
      if (method == 1)
      {
        w = 1;
      }
      else
      {
        double dx =  X[i] - pts[j]->x;
        double dy =  Y[i] - pts[j]->y;
        w = 1/twosquaresigmapi * std::exp(-(dx*dx + dy*dy)/twosquaresigma);
      }

      ztot += w*Z[pts[j]->id];
      wtot += w;
    }

    Z_out[i] = ztot/wtot;

    if (p.check_abort())
    {
      p.exit();
    }

    p.update(i);
  }

  return Z_out;
}
