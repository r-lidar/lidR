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
#include "QuadTree.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector fast_table(IntegerVector x, int size = 5)
{
  IntegerVector tbl(size);

  for (IntegerVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it <= size && *it > 0)
      tbl(*it-1)++;
  }

  return tbl;
}

// [[Rcpp::export]]
int fast_countequal(NumericVector x, double t)
{
  return std::count(x.begin(), x.end(), t);
}

// [[Rcpp::export]]
int fast_countbelow(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind2nd(std::less<int>(), t));
}

// [[Rcpp::export]]
int fast_countover(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind2nd(std::greater<int>(), t));
}

// [[Rcpp::export]]
NumericVector fast_extract(NumericMatrix r, NumericVector x, NumericVector y, double xmin, double ymin, double res)
{
  NumericVector z(x.length());
  int h = r.nrow();
  int w = r.ncol();
  double xmax = xmin + w * res;
  double ymax = ymin + h * res;

  for (int k = 0 ; k < x.length() ; k++)
  {
    double yk = y[k];
    double xk = x[k];

    if (yk < ymin || yk > ymax) {
      z(k) = NumericVector::get_na();
      continue;
    }

    if (xk < xmin || xk > xmax) {
      z(k) = NumericVector::get_na();
      continue;
    }

    if (yk == (int)yk)
      yk = yk-0.01*res;

    double sx = xk - xmin;
    double sy = yk - ymin;

    int j = (int)(std::abs((xmin - xk) / res) + 1)-1;
    int i = r.nrow() - (int)(std::abs((ymin - yk) / res))-1;

    if (j == w)
      j--;

    //Rcpp::Rcout << i << " " << j << std::endl;

    z(k) = r(i, j);
  }

  return(z);
}

// [[Rcpp::export]]
NumericVector roundc(NumericVector x, int digit = 0)
{
  NumericVector y(x.length());
  NumericVector::iterator itx = x.begin();
  NumericVector::iterator ity = y.begin();

  for(itx = x.begin(), ity = y.begin() ; itx != x.end() ; ++itx, ++ity)
  {
    *ity = round(*itx);
  }

  return y;
}

