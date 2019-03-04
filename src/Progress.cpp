#include "myomp.h"
#include "Progress.h"

Progress::Progress(unsigned int iter_max, std::string prefix)
{
  SEXP prgssSEXP = Rf_GetOption(Rf_install("lidR.progress"), R_BaseEnv);
  this->display = Rf_isLogical(prgssSEXP) && (Rcpp::as<bool>(prgssSEXP) == true);

  SEXP delaySEXP = Rf_GetOption(Rf_install("lidR.progress.delay"), R_BaseEnv);
  this->delay = Rcpp::as<float>(delaySEXP);

  iter = 0;
  this->iter_max = iter_max;
  this->prefix = prefix;
  j = 0;
  ti = clock();
  percentage = 0;
}

void Progress::check_abort()
{
  if(omp_get_thread_num() != 0)
    return;

  j++;
  if(j % 1000 != 0)
    return;

  Rcpp::checkUserInterrupt();

  return;
}

bool Progress::check_interrupt()
{
  if(omp_get_thread_num() != 0)
    return false;

  j++;

  if(j % 1000 != 0)
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
  this->iter = iter;

  if (!display)
    return;

  unsigned int p = ((float)iter*omp_get_num_threads())/(float)iter_max*100;

  if (p == percentage)
    return;

  clock_t dt = clock() - ti;
  if( ((float)dt)/CLOCKS_PER_SEC  < delay)
    return;


  percentage = p;

  Rcpp::Rcout << prefix << percentage << "% (" << omp_get_num_threads() <<  " threads)\r";
  Rcpp::Rcout.flush();


  return;
}

void Progress::increment()
{
  if(omp_get_thread_num() != 0)
    return;

  this->iter++;

  if (!display)
    return;

  unsigned int p = ((float)iter*omp_get_num_threads())/(float)iter_max*100;

  if (p == percentage)
    return;

  percentage = p;

  clock_t dt = clock() - ti;
  if( ((float)dt)/CLOCKS_PER_SEC  < delay)
    return;

  int nchar = std::floor((float)(percentage)/2);
  std::string completed = std::string(nchar, '=');
  std::string remaining = std::string(50 - nchar, '-');
  Rcpp::Rcout << prefix << "[" << completed << remaining << "] " << percentage << "% (" << omp_get_num_threads() <<  " threads)\r";
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
