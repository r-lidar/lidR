#ifndef PROGRESS_H
#define PROGRESS_H

#include <Rcpp.h>

class Progress
{
  public:
    Progress(unsigned int, bool);
    ~Progress();
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
    bool display;
    static bool exist;
};

#endif //PROGRESS_H
