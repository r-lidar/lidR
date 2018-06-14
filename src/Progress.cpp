#include "Progress.h"

bool Progress::exist = false;

Progress::Progress(unsigned int _iter_max, bool _display)
{
  if (exist) { Rf_error("Error: there is already an interruptable instance defined"); }

  iter = 0;
  iter_max = _iter_max;
  display = _display;
  j = 0;
  percentage = 0;
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

void Progress::update(unsigned int iter)
{
  if (!display)
    return;

  this->iter = iter;

  int p = (float)iter/(float)iter_max*100;

  if (p == percentage)
    return;

  percentage = p;
  Rcpp::Rcout << percentage << "%\r";
  Rcpp::Rcout.flush();

  return;
}

void Progress::increment()
{
  if (!display)
    return;

  iter++;

  int p = (float)iter/(float)iter_max*100;

  if (p == percentage)
    return;

  percentage = p;
  Rcpp::Rcout << percentage << "%\r";
  Rcpp::Rcout.flush();

  return;
}

unsigned int Progress::get_iter()
{
  return iter;
}

void Progress::exit()
{
  throw Rcpp::internal::InterruptedException();
}