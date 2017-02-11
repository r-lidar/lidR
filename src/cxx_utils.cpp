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

IntegerVector which_true(LogicalVector x)
{
  int n = 0;
  IntegerVector y;

  for (LogicalVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it)
      y.push_back(n);

    n++;
  }

  return y;
}

int which_max(NumericVector x)
{
  int n = -1;
  double max = -INFINITY;

  for (int i = 0, end = x.length() ; i < end ; i++)
  {
    if (x(i) >= max)
    {
      max = x(i);
      n = i;
    }
  }

  return n;
}

// [[Rcpp::export]]
NumericMatrix get_normales(IntegerMatrix M, NumericMatrix X, int size, bool edge_size = false)
{
  NumericMatrix N(size, 5);
  std::fill(N.begin(), N.end(), NA_REAL);

  for(int i = 0, end = M.nrow(); i < end ; i++)
  {
    int p1 = M(i,0)-1;
    int p2 = M(i,1)-1;
    int p3 = M(i,2)-1;
    int j  = M(i,3)-1;

    NumericVector A = NumericVector::create(X(p1,0), X(p1,1), X(p1,2));
    NumericVector B = NumericVector::create(X(p2,0), X(p2,1), X(p2,2));
    NumericVector C = NumericVector::create(X(p3,0), X(p3,1), X(p3,2));

    NumericVector u = A - B;
    NumericVector v = A - C;
    NumericVector w = B - C;

    NumericVector n = NumericVector::create(u(1)*v(2)-u(2)*v(1), u(2)*v(0)-u(0)*v(2), u(0)*v(1)-u(1)*v(0));
    n.push_back(sum(-n*C));

    N(j,0) = n(0);
    N(j,1) = n(1);
    N(j,2) = n(2);
    N(j,3) = n(3);

    if(edge_size)
    {
      u.erase(2);
      v.erase(2);
      w.erase(2);
      NumericVector e = NumericVector::create(sqrt(sum(pow(u, 2))), sqrt(sum(pow(v, 2))), sqrt(sum(pow(w, 2))));
      N(j,4) = max(e);
    }
  }

  return N;
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

NumericVector distance(NumericVector x1, NumericVector y1, double x2, double y2)
{
  NumericVector y = sqrt( pow((x1-x2),2) + pow((y1-y2),2));
  return y;
}

