#ifndef LAS_H
#define LAS_H


#include <RcppArmadillo.h>
#define NDEBUG 1
using namespace Rcpp;

class LAS
{
  public:
    NumericVector X;
    NumericVector Y;
    NumericVector Z;
    IntegerVector I;
    unsigned int ncpu;
    unsigned int npoints;
    std::vector<bool> filter;

  public:
    LAS(S4 las);
    LAS(S4 las, int npcu);
    ~LAS();

    void new_filter(LogicalVector b);
    void apply_filter();
    IntegerVector index_filter();

    void filter_in_polygon(std::string wkt);
    void filter_local_maxima(NumericVector ws, double min_height, bool circular);
    void filter_with_grid(S4 layout);
    void filter_shape(int method, NumericVector th, int k);
    void filter_progressive_morphology(NumericVector ws, NumericVector th);

    void z_open(double resolution);
    void z_smooth(double size, int method, int shape, double sigma);

    NumericVector rasterize(S4 layout, double subcircle, int method);

    IntegerVector segment_snags(NumericVector neigh_radii, double low_int_thrsh, double uppr_int_thrsh, int pt_den_req, NumericMatrix BBPRthrsh_mat);
    IntegerVector segment_trees(double dt1, double dt2, double Zu, double R, double th_tree, double radius);

  private:
    static bool coplanar (arma::vec& latent, arma::mat& coeff, NumericVector& th) { return latent[1] > th[0]*latent[2] && th[1]*latent[1] > latent[0]; }
    static bool hcoplanar(arma::vec& latent, arma::mat& coeff, NumericVector& th) { return latent[1] > th[0]*latent[2] && th[1]*latent[1] > latent[0] && std::abs(coeff(2,2)) > th[2]; }
    static bool colinear (arma::vec& latent, arma::mat& coeff, NumericVector& th) { return th[0]*latent[2] < latent[0] && th[0]*latent[1] < latent[0]; }
    static double rmax(double x, double y) { if (NumericVector::is_na(x)) return y; return (x > y) ? x : y; }
    static double rmin(double x, double y) { if (NumericVector::is_na(x)) return y; return (x < y) ? x : y; }
    static double rcount(double x, double y) { if (NumericVector::is_na(x)) return 1; return x+1;}
};

#endif //LAS_H
