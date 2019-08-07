#ifndef TRIANGULATION_H
#define TRIANGULATION_H

#include <Rcpp.h>
using namespace Rcpp;

class Triangulator
{
  public:
    IntegerMatrix D;
    NumericMatrix P;
    int ncpu;

  public:
    Triangulator(IntegerMatrix D, NumericMatrix P);
    Triangulator(IntegerMatrix D, NumericMatrix P, int ncpu);
    NumericMatrix info();
    IntegerVector search(NumericMatrix X);
};

#endif //TRIANGULATION_H
