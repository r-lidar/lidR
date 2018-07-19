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
          max(seed_neighbours) == 0 &&                           // And there is no other seed in the neighborhood
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
LogicalVector C_LocalMaximaPoints(S4 las, double ws, double min_height)
{
  DataFrame data  = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  int n = X.length();
  double hws = ws/2;

  LogicalVector seeds(n);
  QuadTree *tree = QuadTreeCreate(X,Y);

  // Loop through all the point cloud
  for (int i = 0 ; i < n ; i++)
  {
    if (Z[i] <= min_height)
      continue;

    // Get the points within a windows centered on the current point
    std::vector<Point*> pts;
    tree->rect_lookup(X[i], Y[i], hws, hws, pts);

    // Get the highest Z in the windows
    double Zmax = std::numeric_limits<double>::min();
    Point *p;
    for(size_t j = 0 ; j < pts.size() ; j++)
    {
      if(Z[pts[j]->id] > Zmax)
      {
        p = pts[j];
        Zmax = Z[p->id];
      }
    }

    // The central pixel is the highest, it is a LM
    if (Z[i] == Zmax && X[i] == p->x && Y[i] == p->y)
      seeds[i] = true;
  }

  delete tree;
  return seeds;
}