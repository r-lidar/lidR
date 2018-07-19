#ifndef PROGRESS_H
#define PROGRESS_H

#include <Rcpp.h>

class Progress
{
  public:
    Progress(int, bool);
    ~Progress();
    bool check_abort();
    void update(int);
    void exit();

  private:
    int iter_max;
    int percentage;
    int j;
    bool display;
    static bool exist;
};

#endif //PROGRESS_H
