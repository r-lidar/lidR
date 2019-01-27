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
#include "QuadTree.h"
#include "Progress.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List C_knn(NumericVector X, NumericVector Y, NumericVector x, NumericVector y, int k)
{
  unsigned int n = x.length();
  IntegerMatrix knn_idx(n, k);
  NumericMatrix knn_dist(n, k);

  QuadTree tree(X,Y);

  for(unsigned int i = 0 ; i < n ; i++)
  {
    Point pt(x[i], y[i]);
    std::vector<Point*> pts;
    tree.knn(pt, k, pts);

    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      knn_idx(i, j)  = pts[j]->id + 1;

      double dx = pts[j]->x - x[i];
      double dy = pts[j]->y - y[i];

      knn_dist(i, j) =  std::sqrt(dx*dx + dy*dy);
    }
  }

  return Rcpp::List::create(Rcpp::Named("nn.idx") = knn_idx, Rcpp::Named("nn.dist") = knn_dist);
}

// [[Rcpp::export]]
NumericVector C_knnidw(NumericVector X, NumericVector Y, NumericVector Z, NumericVector x, NumericVector y, int k, double p)
{
  unsigned int n = x.length();
  NumericVector iZ(n);

  QuadTree tree(X,Y);

  Progress pbar(n, "Inverse distance weighting: ");

  for(unsigned int i = 0 ; i < n ; i++)
  {
    Point pt(x[i], y[i]);
    std::vector<Point*> pts;
    tree.knn(pt, k, pts);

    double sum_zw = 0;
    double sum_w  = 0;

    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      double dx = pts[j]->x - x[i];
      double dy = pts[j]->y - y[i];
      double d  = std::sqrt(dx*dx + dy*dy);
      double w;
      double z = Z[pts[j]->id];

      if (d > 0)
      {
        w = 1/pow(d,p);
        sum_zw += z*w;
        sum_w  += w;
      }
      else
      {
        sum_zw = z;
        sum_w  = 1;
        break;
      }
    }

    iZ(i) = sum_zw/sum_w;

    if (pbar.check_abort())
    {
      pbar.exit();
    }

    pbar.update(i);
  }

  return iZ;
}
