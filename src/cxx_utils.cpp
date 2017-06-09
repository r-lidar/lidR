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
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it == t)
      n++;
  }

  return n;
}

// [[Rcpp::export]]
int fast_countbelow(NumericVector x, double t)
{
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it < t)
      n++;
  }

  return n;
}

// [[Rcpp::export]]
int fast_countover(NumericVector x, double t)
{
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it > t)
      n++;
  }

  return n;
}

IntegerMatrix which_equal(IntegerMatrix mtx, double val)
{
  int l = mtx.nrow();
  int w = mtx.ncol();

  NumericVector x;
  NumericVector y;

  for(int i = 0 ; i < l ; i++)
  {
    for(int j = 0 ; j < w ; j++)
    {
      if(mtx(i,j) == val)
      {
        x.push_back(i);
        y.push_back(j);
      }
    }
  }

  IntegerMatrix m(x.length(), 2);
  m(_, 0) = x;
  m(_, 1) = y;

  return(m);
}

NumericVector filter_xx(NumericMatrix x, IntegerMatrix y)
{
  int nrow = y.nrow();
  NumericVector out(nrow);

  for(int i = 0 ; i < nrow ; i++)
    out(i) = x(y(i,0), y(i,1));

  return(out);
}

NumericVector sqdistance(NumericVector x1, NumericVector y1, double x2, double y2)
{
  int n = x1.length();
  NumericVector y(n);
  NumericVector::iterator i1, i2, i3, end1, end2, end3;

  for( i1 = x1.begin(), i2 = y1.begin(), i3 = y.begin(), end1 = x1.end(), end2 = y1.end(), end3 = y.end();
       i1 < end1 && i2 < end2 && i3 < end3;
        ++i1, ++i2 , ++i3)
  {
    double dx = *i1-x2;
    double dy = *i2-y2;
    *i3 = dx * dx + dy * dy;
  }

  return y;
}

