#include "Progress.h"

bool Progress::exist = false;

Progress::Progress(int _iter_max, bool _display) : iter_max(_iter_max), display(_display), j(0), percentage(-1)
{
  if (exist) { Rf_error("Error: there is already an interruptable instance defined"); }
  exist = true;
}

Progress::~Progress() { exist = false; }

bool Progress::check_abort()
{
  j++;

  if(j % 100 != 0)
    return false;

  try
  {
    Rcpp::checkUserInterrupt();
  }
  catch(Rcpp::internal::InterruptedException e)
  {
    return true;
  }

  return false;
}

void Progress::update(int iter)
{
  if (!display)
    return;

  int p = (float)iter/(float)iter_max*100;

  if (p == percentage)
    return;

  percentage = p;
  Rcpp::Rcout << percentage << "%\r";
  Rcpp::Rcout.flush();

  return;
}

void Progress::exit()
{
  throw Rcpp::internal::InterruptedException();
}