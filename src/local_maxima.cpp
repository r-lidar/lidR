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

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include <limits>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix LocalMaximaMatrix(NumericMatrix Canopy, double searchWinSize)
{
  int l = Canopy.nrow();
  int w = Canopy.ncol();

  int r, k, minR, minC, maxR, maxC;
  int index = 1;

  double hws  = searchWinSize/2;
  int    fhws = floor(hws);

  NumericMatrix FIL;
  IntegerMatrix temp;

  IntegerMatrix Maxima(l, w);

  for (int r = fhws ; r < l-fhws ; r++)
  {
    for(int k = fhws ; k <  w-fhws ; k++)
    {
      minR = (r - fhws);
      minC = (k - fhws);
      maxR = (r + fhws);
      maxC = (k + fhws);

      FIL  = Canopy(Range(minR,maxR), Range(minC,maxC));
      temp = Maxima(Range(minR,maxR), Range(minC,maxC));

      if (FIL(fhws,fhws) == max(FIL) && max(temp) == 0 && max(FIL) != 0)
      {
        Maxima(r,k) = index;
        index++;
      }
    }
  }

  return(Maxima);
}

// [[Rcpp::export]]
LogicalVector LocalMaximaPoints(NumericVector X, NumericVector Y, NumericVector Z, double radius, bool displaybar = false)
{
  int n = X.length();

  LogicalVector is_maxima(n);
  LogicalVector isnot_maxima(n);

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  Progress p(n, displaybar);

  // erode
  for (long i = 0 ; i < n ; i++)
  {
    if (Progress::check_abort() )
      return is_maxima;
    else
      p.update(i);

    std::vector<Point*> pts;
    tree->rect_lookup(X[i], Y[i], radius, radius, pts);

    long id_new_max = -1;
    long id_old_max = -1;
    double max(std::numeric_limits<double>::min());

    for(int j = 0 ; j < pts.size() ; j++)
    {
      long pid = pts[j]->id;

      double z = Z[pid];

      if(z > max && !isnot_maxima[pid])
      {
        max = z;
        id_new_max = pid;
      }

      if (is_maxima[pid])
      {
        id_old_max = pid;
      }
    }

    for(int j = 0 ; j < pts.size() ; j++)
    {
      long pid = pts[j]->id;

      if (pid != id_new_max)
        isnot_maxima[pid] = true;
    }

    if(id_old_max != -1)
      is_maxima[id_old_max] = false;

    if(id_new_max != -1)
      is_maxima[id_new_max] = true;
  }

  delete tree;

  return is_maxima;
}
