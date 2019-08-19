#include "Triangulation.h"
#include "QuadTree.h"
#include "Progress.h"
#include "myomp.h"

Triangulator::Triangulator(IntegerMatrix D, NumericMatrix P)
{
  this->D = D;
  this->P = P;
  this->ncpu = 1;
}

Triangulator::Triangulator(IntegerMatrix D, NumericMatrix P, int ncpu)
{
  this->D = D;
  this->P = P;
  this->ncpu = ncpu;
}

NumericMatrix Triangulator::info()
{
  if (P.nrow() < 3)  Rcpp::stop("Internal error in 'info()': wrong dimension for P");

  NumericMatrix N(D.nrow(), 7);
  std::fill(N.begin(), N.end(), NA_REAL);

  for (int i = 0, end = D.nrow() ; i < end ; i++)
  {
    int p1 = D(i,0)-1;
    int p2 = D(i,1)-1;
    int p3 = D(i,2)-1;

    NumericVector A = NumericVector::create(P(p1,0), P(p1,1), P(p1,2));
    NumericVector B = NumericVector::create(P(p2,0), P(p2,1), P(p2,2));
    NumericVector C = NumericVector::create(P(p3,0), P(p3,1), P(p3,2));

    NumericVector u = A - B;
    NumericVector v = A - C;
    NumericVector w = B - C;

    // Cross product
    NumericVector n(3);
    n(0) = u(1)*v(2)-u(2)*v(1);
    n(1) = u(2)*v(0)-u(0)*v(2);
    n(2) = u(0)*v(1)-u(1)*v(0);

    // normal vector
    N(i,0) = n(0);
    N(i,1) = n(1);
    N(i,2) = n(2);

    // intercept
    N(i,3) = sum(-n*C);

    // area and projected area
    N(i,4) = std::fabs(0.5 * sqrt(n(0) * n(0) + n(1) * n(1) + n(2) * n(2)));
    N(i,5) = std::fabs(0.5 * n(2));

    // max edge length
    u.erase(2);
    v.erase(2);
    w.erase(2);
    NumericVector e = NumericVector::create(sqrt(sum(pow(u, 2))), sqrt(sum(pow(v, 2))), sqrt(sum(pow(w, 2))));
    N(i,6) = max(e);
  }

  colnames(N) = CharacterVector::create("nx", "ny", "nz", "intercept", "xyzarea", "xyarea", "maxedge");
  return N;
}

IntegerVector Triangulator::search(NumericMatrix X)
{
  NumericVector x = X(_, 0);
  NumericVector y = X(_, 1);
  QuadTree tree(x, y);

  int nelem = D.nrow();
  int np = X.nrow();

  bool abort = false;

  Progress pb(nelem, "Searching in TIN: ");

  IntegerVector output(np);
  std::fill(output.begin(), output.end(), NA_INTEGER);

  // Loop over each triangle
  #pragma omp parallel for num_threads(ncpu)
  for (int k = 0; k < nelem; k++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    // Retrieve triangle A B C coordinates
    int iA = D(k, 0) - 1;
    int iB = D(k, 1) - 1;
    int iC = D(k, 2) - 1;

    Point A(P(iA, 0), P(iA, 1));
    Point B(P(iB, 0), P(iB, 1));
    Point C(P(iC, 0), P(iC, 1));

    Triangle triangle(A,B,C);
    std::vector<Point*> points;
    tree.lookup(triangle, points);

    // Return the id of the triangle
    #pragma omp critical
    {
      for(std::vector<Point*>::iterator it = points.begin(); it != points.end(); it++)
      {
        int id = (*it)->id;
        output(id) = k + 1;
      }
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return(output);
}
