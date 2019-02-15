/*
===============================================================================

PROGRAMMERS:

jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

COPYRIGHT:

Copyright 2016 Jean-Romain Roussel

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
NumericVector C_MorphologicalOpening(NumericVector X, NumericVector Y, NumericVector Z, double resolution)
{
  unsigned int n = X.length();
  double half_res = resolution / 2;

  NumericVector Z_temp = clone(Z);
  NumericVector Z_out  = clone(Z);

  QuadTree tree(X,Y);

  Progress p(2*n, "Morphological filter: ");

  // Dilate
  for (unsigned int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double min_pt(std::numeric_limits<double>::max());

    for(unsigned  int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j]->id];

      if(z < min_pt)
        min_pt = z;
    }

    Z_out[i] = min_pt;

    p.check_abort();
    p.update(i);
  }

  Z_temp = clone(Z_out);

  // erode
  for (unsigned int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double max_pt(std::numeric_limits<double>::min());

    for(unsigned int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j]->id];

      if(z > max_pt)
        max_pt = z;
    }

    Z_out[i] = max_pt;

    p.check_abort();
    p.update(i+n);
  }

  return Z_out;
}
