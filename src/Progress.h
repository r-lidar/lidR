#ifndef PROGRESS_H
#define PROGRESS_H

#include <Rcpp.h>
#include <time.h>

class Progress
{
  public:
    Progress(unsigned int, std::string = "");
    bool check_abort();
    void update(unsigned int);
    void increment();
    void exit();
    unsigned int get_iter();

  private:
    unsigned int iter;
    unsigned int iter_max;
    unsigned int percentage;
    unsigned int j;
    std::string prefix;
    clock_t ti;
    bool display;
};

#endif //PROGRESS_H
