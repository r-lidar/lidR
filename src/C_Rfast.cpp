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

#include <RcppArmadillo.h>
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
int fast_countequal(IntegerVector x, int t)
{
  return std::count(x.begin(), x.end(), t);
}

// [[Rcpp::export]]
int fast_countbelow(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind2nd(std::less<double>(), t));
}

// [[Rcpp::export]]
int fast_countover(NumericVector x, double t)
{
  return std::count_if(x.begin(), x.end(), std::bind2nd(std::greater<double>(), t));
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

// [[Rcpp::export]]
SEXP fast_eigen_values(arma::mat A)
{
  arma::mat coeff;
  arma::mat score;
  arma::vec latent;
  arma::princomp(coeff, score, latent, A);
  NumericMatrix eigenvalues = Rcpp::wrap(latent);
  NumericMatrix eigencoeff = Rcpp::wrap(coeff);
  return(List::create(_["eigen"] = eigenvalues, _["coeff"] = eigencoeff));
}

