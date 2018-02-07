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
IntegerMatrix C_LocalMaximaMatrix(NumericMatrix image, int ws, double th)
{
  int nrow = image.nrow();
  int ncol = image.ncol();

  int minR, minC, maxR, maxC;
  int index = 1;
  int fhws = floor((double)ws/2);

  NumericMatrix img_neighbours;
  IntegerMatrix seed_neighbours;
  IntegerMatrix seeds(nrow, ncol);

  for (int r = 0 ; r < nrow ; r++)
  {
    for(int k = 0 ; k <  ncol ; k++)
    {
      minR = (r - fhws);
      minC = (k - fhws);
      maxR = (r + fhws);
      maxC = (k + fhws);

      minR = minR >= 0 ? minR : 0;
      minC = minC >= 0 ? minC : 0;
      maxR = maxR < nrow ? maxR : nrow-1;
      maxC = maxC < ncol ? maxC : ncol-1;

      img_neighbours  = image(Range(minR,maxR), Range(minC,maxC));
      seed_neighbours = seeds(Range(minR,maxR), Range(minC,maxC));

      if (image(r,k) == max(img_neighbours) &&                   // If center pixel is the highest
          max(seed_neighbours) == 0 &&                           // And there is not orther seed in the neighborhood
          image(r,k) > th)                                       // And this maximum is not too low
      {
        seeds(r,k) = index;                                      // Then this pixel is a seed
        index++;
      }
    }
  }

  return(seeds);
}

// [[Rcpp::export]]
LogicalVector C_LocalMaximaPoints(S4 las, double ws, double min_height, bool displaybar = false)
{
  // DataFrame data = las.slot("data");
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  int n = X.length();
  double hws = ws/2;

  LogicalVector is_maxima(n);
  LogicalVector isnot_maxima(n);

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  Progress p(n, displaybar);

  for (long i = 0 ; i < n ; i++)
  {
    if (Progress::check_abort() )
      return is_maxima;
    else
      p.update(i);

    std::vector<Point*> pts;
    tree->rect_lookup(X[i], Y[i], hws, hws, pts);

    long id_new_max = -1;
    long id_old_max = -1;
    double max(std::numeric_limits<double>::min());

    for(int j = 0 ; j < pts.size() ; j++)
    {
      long pid = pts[j]->id;

      double z = Z[pid];

      if(z >= min_height && z > max && !isnot_maxima[pid])
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

