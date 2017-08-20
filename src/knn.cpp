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
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List knn(NumericVector X, NumericVector Y, NumericVector x, NumericVector y, int k)
{
  int n = x.length();
  NumericMatrix knn_idx(n, k);
  NumericMatrix knn_dist(n, k);

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  for( int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    tree->knn_lookup(x[i], y[i], k, pts);

    for (int j = 0 ; j < pts.size() ; j++)
    {
      knn_idx(i, j)  = pts[j]->id + 1;

      double dx = pts[j]->x - x[i];
      double dy = pts[j]->y - y[i];

      knn_dist(i, j) =  std::sqrt(dx*dx + dy*dy);
    }
  }

  return Rcpp::List::create(Rcpp::Named("nn.idx") = knn_idx,
                            Rcpp::Named("nn.dist") = knn_dist);
}

// [[Rcpp::export]]
NumericVector knnidw(NumericVector X, NumericVector Y, NumericVector Z, NumericVector x, NumericVector y, int k)
{
  int n = x.length();
  NumericVector iZ(n);

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  for( int i = 0 ; i < n ; i++)
  {
    std::vector<Point*> pts;
    tree->knn_lookup(x[i], y[i], k, pts);

    double sum_zw = 0;
    double sum_w  = 0;

    for (int j = 0 ; j < pts.size() ; j++)
    {
      double dx = pts[j]->x - x[i];
      double dy = pts[j]->y - y[i];
      double d  = std::sqrt(dx*dx + dy*dy);
      double w;
      double z = Z[pts[j]->id];

      if (d > 0)
      {
        w = 1/d;
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
  }

  return iZ;
}
