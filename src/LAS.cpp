#include "LAS.h"
#include <boost/geometry.hpp>
#include <limits>
//#include "QuadTree.h"
#include "GridPartition.h"
#include "Progress.h"
#include "myomp.h"

typedef GridPartition SpatialIndex;

LAS::LAS(S4 las)
{
  DataFrame data = as<DataFrame>(las.slot("data"));
  this->X = data["X"];
  this->Y = data["Y"];
  this->Z = data["Z"];

  if (data.containsElementNamed("Intensity"))
    this->I = data["Intensity"];

  if (data.containsElementNamed("gpstime"))
    this->T = data["gpstime"];

  this->npoints = X.size();
  this->ncpu = 1;
  this->filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), false);
}

LAS::LAS(S4 las, int ncpu)
{
  DataFrame data = as<DataFrame>(las.slot("data"));
  this->X = data["X"];
  this->Y = data["Y"];
  this->Z = data["Z"];

  if (data.containsElementNamed("Intensity"))
    this->I = data["Intensity"];

  this->npoints = X.size();
  this->ncpu = ncpu;
  this->filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), false);
}

LAS::~LAS()
{
}

void LAS::new_filter(LogicalVector b)
{
  if (b.size() == 1)
    std::fill(filter.begin(), filter.end(), b[0]);
  else if (b.size() == (int)npoints)
    this->filter = Rcpp::as< std::vector<bool> >(b);
  else
    Rcpp::stop("Internal error in 'new_filter"); // # nocov
}

/*void LAS::apply_filter()
{
  LogicalVector keep = wrap(filter);

  X = X[keep];
  Y = Y[keep];
  Z = Z[keep];

  if (I.size() == filter.size())
    I = I[keep];

  npoints = X.size();
  filter = std::vector<bool>(npoints);
  std::fill(filter.begin(), filter.end(), false);
}*/

/*IntegerVector LAS::index_filter()
{
  std::vector<int> index;
  for (int i = 0 ; i < npoints ; i++)
  {
    if (filter[i]) index.push_back(i+1);
  }

  return Rcpp::wrap(index);
}*/

void LAS::z_smooth(double size, int method, int shape, double sigma)
{
  // shape: 1- rectangle 2- circle
  // method: 1- average 2- gaussian
  double half_res = size / 2;
  double twosquaresigma = 2*sigma*sigma;
  double twosquaresigmapi = twosquaresigma * M_PI;

  NumericVector Zsmooth = clone(Z);

  SpatialIndex tree(X,Y);

  Progress pb(npoints, "Point cloud smoothing: ");

  bool abort = false;

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    std::vector<Point*> pts;

    if(shape == 1)
    {
      Rectangle rect(X[i]-half_res, X[i]+half_res, Y[i]-half_res,  Y[i]+half_res);
      tree.lookup(rect, pts);
    }
    else
    {
      Circle circ(X[i], Y[i], half_res);
      tree.lookup(circ, pts);
    }

    double w = 0;
    double ztot = 0;
    double wtot = 0;

    for(unsigned int j = 0 ; j < pts.size() ; j++)
    {
      if (method == 1)
      {
        w = 1;
      }
      else
      {
        double dx = X[i] - pts[j]->x;
        double dy = Y[i] - pts[j]->y;
        w = 1/twosquaresigmapi * std::exp(-(dx*dx + dy*dy)/twosquaresigma);
      }

      ztot += w*Z[pts[j]->id];
      wtot += w;
    }

    #pragma omp critical
    {
      Zsmooth[i] = ztot/wtot;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  Z = Zsmooth;
  return;
}

void LAS::z_open(double resolution)
{
  double half_res = resolution / 2;

  NumericVector Z_out(npoints);

  SpatialIndex tree(X, Y, filter);

  Progress p(2*npoints, "Morphological filter: ");

  // Dilate
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    p.check_abort();
    p.update(i);
    if (!filter[i]) continue;

    std::vector<Point*> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double min_pt(std::numeric_limits<double>::max());

    for(unsigned  int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z[pts[j]->id];

      if(z < min_pt)
        min_pt = z;
    }

    Z_out[i] = min_pt;
  }

  NumericVector Z_temp = clone(Z_out);

  // erode
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    p.check_abort();
    p.update(i+npoints);
    if (!filter[i]) continue;

    std::vector<Point*> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double max_pt(std::numeric_limits<double>::min());

    for(unsigned int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j]->id];

      if(z > max_pt)
        max_pt = z;
    }

    Z_out[i] = max_pt;
  }

  Z = Z_out;
  return;
}

void LAS::i_range_correction(DataFrame flightlines, double Rs, double f)
{
  // Coordinates of the sensors
  NumericVector x = flightlines["X"];
  NumericVector y = flightlines["Y"];
  NumericVector z = flightlines["Z"];
  NumericVector t = flightlines["gpstime"];

  // Compute the median sensor elevation then average range for this sensor
  // elevation. This gives a rough idea of the expected range and allows for
  // detecting failure and bad computations
  double median_z_sensor = Rcpp::median(z);
  double R_control = mean(median_z_sensor - Z);

  NumericVector::iterator it;
  double dx, dy, dz, r, R;
  double i;
  int j;

  IntegerVector Inorm(X.size());

  Progress pbar(npoints, "Range computation");

  // Loop on each point
  for (unsigned int k = 0 ; k < npoints ; k++)
  {
    pbar.increment();
    pbar.check_abort();

    // The sensor positions were already sorted a R level
    // For each point find the first element that is not less than the time t of the points
    // This give the closest position of the sensor after (t1) the aqcuisition of the points (t)
    it = std::lower_bound(t.begin(), t.end(), T[k]);

    // We now need the sensor position before (t0) the aqcuisition.

    // If the sensor position is the first one: no sensor position exists before this one
    // thus no interpolation possible. We use the next one.
    if (it == t.begin())
    {
      j = 1;
    }
    // If the sensor position not found: no sensor position exists after this one
    // thus no interpolation possible. We use the last one.
    else if (it == t.end())
    {
      j = x.size() - 1;
    }
    // If t1-t0 is too big it is two differents flightlines. We are actually in the same case than
    // above but in a new flightline. No interpolation with the previous one (edge of data).
    // We use the next one
    else if (*it - *(it-1) > 30)
    {
      j = it - t.begin() + 1;
    }
    // General case with t1 > t > t0. We have a sensor position after the aquisition of the point
    // and it is not the first one. So we necessarily have a previous one. We can make the
    // interpolation
    else
    {
      j = it - t.begin();
    }

    if (j >= x.size()) throw Rcpp::exception("Internal error: access to coordinates beyond the limits of the array. Please report this bug.", false);
    if (j <= 0)        throw Rcpp::exception("Internal error: access to coordinates below 0 in the array. Please report this bug.", false);

    r  = 1 - (t[j]-T[k])/(t[j]-t[j-1]);
    dx = X[k] - (x[j-1] + (x[j] - x[j-1])*r);
    dy = Y[k] - (y[j-1] + (y[j] - y[j-1])*r);
    dz = Z[k] - (z[j-1] + (z[j] - z[j-1])*r);
    R  = std::sqrt(dx*dx + dy*dy + dz*dz);

    if (R > 3 * R_control)
    {
      REprintf("An high range R has been computed relatively to the expected average range Rm = %.0lf\n", R_control);
      REprintf("Point number %d at (x,y,z,t) = (%.2lf, %.2lf, %.2lf, %.2lf)\n", k+1, X[k], Y[k], Z[k], T[k]);
      REprintf("Matched with sensor between (%.2lf, %.2lf, %.2lf, %.2lf) and (%.2lf, %.2lf, %.2lf, %.2lf)\n", x[j-1], y[j-1], z[j-1], t[j-1], x[j], y[j], z[j], t[j]);
      REprintf("The range computed was R = %.2lf\n", R, dx, dy, dz, t[j]);
      REprintf("Check the correctness of the sensor positions and the correctness of the gpstime either in the point cloud or in the sensor positions.\n");
      throw Rcpp::exception("Unrealistic range: see message above", false);
    }

    i = I[k] * std::pow((R/Rs),f);

    if (i > 65535)
    {
      Rf_warningcall(R_NilValue, "Normalized intensity does not fit in 16 bit. Value clamped to 2^16.");
      i = 65535;
    }

    Inorm[k]  = i;
  }

  I = Inorm;

  return;
}

void LAS::filter_local_maxima(NumericVector ws, double min_height, bool circular)
{
  bool abort = false;
  bool vws = ws.length() > 1;

  SpatialIndex tree(X,Y);
  Progress pb(npoints, "Local maximum filter: ");

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    double hws = (vws) ? ws[i]/2 : ws[0]/2;

    if (Z[i] < min_height)
      continue;

    // Get the points within a windows centered on the current point
    std::vector<Point*> pts;
    if(!circular)
    {
      Rectangle rect(X[i]-hws, X[i]+hws, Y[i]-hws, Y[i]+hws);
      tree.lookup(rect, pts);
    }
    else
    {
      Circle circ(X[i], Y[i], hws);
      tree.lookup(circ, pts);
    }

    // Get the highest Z in the windows
    double Zmax = std::numeric_limits<double>::min();
    Point* p = pts[0];
    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      if(Z[pts[j]->id] > Zmax)
      {
        p = pts[j];
        Zmax = Z[p->id];
      }
    }

    // The central pixel is the highest, it is a LM
    #pragma omp critical
    {
      if (Z[i] == Zmax && X[i] == p->x && Y[i] == p->y)
        filter[i] = true;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return;
}

void LAS::filter_with_grid(S4 layout)
{
  S4 extent   = layout.slot("extent");
  int ncols   = layout.slot("ncols");
  int nrows   = layout.slot("nrows");
  double xmin = extent.slot("xmin");
  double xmax = extent.slot("xmax");
  double ymin = extent.slot("ymin");
  double ymax = extent.slot("ymax");
  double xres = (xmax - xmin) / ncols;
  double yres = (ymax - ymin) / nrows;

  std::vector<int> output(ncols*nrows);
  std::fill(output.begin(), output.end(), std::numeric_limits<int>::min());

  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (filter[i]) continue;

    double x = X[i];
    double y = Y[i];
    double z = Z[i];

    int col = std::floor((x - xmin) / xres);
    int row = std::floor((ymax - y) / yres);
    if (y == ymin) row = nrows-1;
    if (x == xmax) col = ncols-1;

    if (row < 0 || row >= nrows || col < 0 || col >= ncols)
      Rcpp::stop("C++ unexpected internal error in 'filter_with_grid': point out of raster."); // # nocov

    int cell = row * ncols + col;

    if (output[cell] == std::numeric_limits<int>::min())
    {
      output[cell] = i;
    }
    else
    {
      double zref = Z[output[cell]];
      if (zref < z) output[cell] = i;
    }
  }

  for (unsigned int i = 0 ; i < output.size() ; i++)
  {
    if (output[i] > std::numeric_limits<int>::min())
      filter[output[i]] = true;
  }

  return;
}

void LAS::filter_in_polygon(std::string wkt)
{
  typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> Point;
  typedef boost::geometry::model::polygon<Point> Polygon;
  typedef boost::geometry::model::multi_polygon<Polygon> MultiPolygon;
  typedef boost::geometry::model::box<Point> Bbox;

  if (wkt.find("MULTIPOLYGON") != std::string::npos)
  {
    Point p;
    Bbox bbox;
    MultiPolygon polygons;

    boost::geometry::read_wkt(wkt, polygons);
    boost::geometry::envelope(polygons, bbox);

    #pragma omp parallel for num_threads(ncpu)
    for(unsigned int i = 0 ; i < npoints ; i++)
    {
      if (filter[i]) continue;

      Point p;
      p.set<0>(X[i]);
      p.set<1>(Y[i]);

      bool isin = false;

      if (boost::geometry::covered_by(p, bbox))
      {
        if (boost::geometry::covered_by(p, polygons))
          isin = true;
      }

      #pragma omp critical
      {
        filter[i] = isin;
      }
    }
  }
  else if (wkt.find("POLYGON") != std::string::npos)
  {
    Point p;
    Bbox bbox;
    Polygon polygon;

    boost::geometry::read_wkt(wkt, polygon);
    boost::geometry::envelope(polygon, bbox);

    #pragma omp parallel for num_threads(ncpu)
    for(unsigned int i = 0 ; i < npoints ; i++)
    {
      if (filter[i]) continue;

      Point p;
      p.set<0>(X[i]);
      p.set<1>(Y[i]);

      bool isin = false;

      if (boost::geometry::covered_by(p, bbox))
      {
        if (boost::geometry::covered_by(p, polygon))
          isin = true;
      }

      #pragma omp critical
      {
        filter[i] = isin;
      }
    }
  }
  else
    Rcpp::stop("Unexpected error in point in polygon: WKT is not a POLYGON or MULTIPOLYGON"); // # nocov

  return;
}

void LAS::filter_shape(int method, NumericVector th, int k)
{
  Progress pb(npoints, "Eigenvalues computation: ");

  bool abort = false;

  SpatialIndex qtree(X,Y,Z, filter);

  bool (*predicate)(arma::vec&, arma::mat&, NumericVector&);
  switch(method)
  {
    case 1: predicate = &LAS::coplanar; break;
    case 2: predicate = &LAS::hcoplanar; break;
    case 3: predicate = &LAS::colinear; break;
  }

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true; // No data race here because only thread 0 can actually write
    pb.increment();
    if (!filter[i]) continue;

    arma::mat A(k,3);
    arma::mat coeff;  // Principle component matrix
    arma::mat score;
    arma::vec latent; // Eigenvalues in descending order

    PointXYZ p(X[i], Y[i], Z[i]);

    std::vector<PointXYZ> pts;
    qtree.knn(p, k, pts);

    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      A(j,0) = pts[j].x;
      A(j,1) = pts[j].y;
      A(j,2) = pts[j].z;
    }

    arma::princomp(coeff, score, latent, A);

    #pragma omp critical
    {
      filter[i] = predicate(latent, coeff, th);
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return;
}

void LAS::filter_progressive_morphology(NumericVector ws, NumericVector th)
{
  if (ws.size() != th.size())
    Rcpp::stop("Internal error in 'filter_progressive_morphology'"); // # nocov

  for (int i = 0 ; i < ws.size() ; i++)
  {
    NumericVector oldZ = clone(Z);
    z_open(ws[i]);

    for (unsigned int j = 0 ; j < npoints ; j++)
    {
      if (!filter[j]) continue;
      filter[j] = (oldZ[j] - Z[j]) < th[i];
    }
  }

  return;
}

IntegerVector LAS::segment_snags(NumericVector neigh_radii, double low_int_thrsh, double uppr_int_thrsh, int pt_den_req, NumericMatrix BBPRthrsh_mat)
{
  NumericVector BBPr_sph(npoints);          // vector to store the Branch-Bole point ratio (BBPr) for the sphere neighborhood object
  IntegerVector ptDen_sph(npoints);         // vector to store the the sphere neighborhood point density for each focal point
  NumericVector meanBBPr_sph(npoints);      // vector to store the mean BBPr for each focal point in its corresponding sphere neighborhood

  NumericVector BBPr_smcyl(npoints);        // BBPr for the small cylinder neighborhood (which only includes points above focal point)
  IntegerVector ptDen_smcyl(npoints);       // the small cylinder neighborhood point density for each focal point
  NumericVector meanBBPr_smcyl(npoints);    // the mean BBPr for each focal point in its corresponding small cylinder neighborhood

  NumericVector BBPr_bigcyl(npoints);       // BBPr for the big cylinder neighborhood
  IntegerVector ptDen_bigcyl(npoints);      // the big cylinder neighborhood point density for each focal point
  NumericVector meanBBPr_bigcyl(npoints);   // the mean BBPr for each focal point in its corresponding big cylinder neighborhood

  SpatialIndex qtree(X,Y,Z);                // the SpatialIndex for the las object

  // Step 1 - First we have to build neighborhood objects (sphere, small and large cylinders) around each focal point and get
  // the BBPr counts, then we have to calculate the actual ratio of BBPr to neighborhood points for each focal point
  // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    double BBPr_cnt = 0;                              // the count of BBPr points (based on thresholds) in the neighborhood

    // Step 1.a Sphere neighborhood
    // ----------------------------

    std::vector<PointXYZ> sphpts;                     // creation of an STL container of points for the sphere neighborhood object
    Sphere sphere(X[i], Y[i], Z[i], neigh_radii[0]);  // creation of a sphere object
    qtree.lookup(sphere, sphpts);                     // lookup the points in the sphere neighborhood
    ptDen_sph[i] = sphpts.size();                     // count the points in the sphere neighborhood

    BBPr_cnt = 0;
    for (unsigned int j = 0 ; j < sphpts.size() ; j++)
    {
      if (I[sphpts[j].id] <= low_int_thrsh || I[sphpts[j].id] >= uppr_int_thrsh)
        BBPr_cnt++;
    }

    #pragma omp critical
    {
      BBPr_sph[i] = BBPr_cnt/sphpts.size();           // Ratio of BBPr points in the neighborhood
    }

    // Step 1.b Small cylinder neighborhood
    // ------------------------------------

    std::vector<Point*> smcylpts;                     // creation of an STL container of points for the small cylinder neighborhood object
    Circle smcircle(X[i], Y[i], neigh_radii[1]);      // creation of a small cylinder object
    qtree.lookup(smcircle, smcylpts);                 // lookup the points in the small cylinder neighborhood

    BBPr_cnt = 0;
    double ptZ = Z[i];                                // the height of the focal point (lower end of the small cylinder)
    for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
    {
      if (Z[smcylpts[j]->id] >= ptZ)
      {
        ptDen_smcyl[i]++;
        if (I[smcylpts[j]->id] <= low_int_thrsh || I[smcylpts[j]->id] >= uppr_int_thrsh)
          BBPr_cnt++;
      }
    }

    #pragma omp critical
    {
      BBPr_smcyl[i] = BBPr_cnt/ptDen_smcyl[i];        // Ratio of BBPr points in the neighborhood
    }

    // Step 1.c Big cylinder neighborhood
    // ----------------------------------

    std::vector<Point*> bigcylpts;                    // creation of an STL container of points for the big cylinder neighborhood object
    Circle bigcircle(X[i], Y[i], neigh_radii[2]);     // creation of a big cylinder object
    qtree.lookup(bigcircle, bigcylpts);               // lookup the points in the big cylinder neighborhood
    ptDen_bigcyl[i] = bigcylpts.size();               // get the point density in the big cylinder neighborhood

    BBPr_cnt = 0;
    for (unsigned int j = 0; j < bigcylpts.size(); j++)
    {
      if (I[bigcylpts[j]->id] <= low_int_thrsh || I[bigcylpts[j]->id] >= uppr_int_thrsh)
        BBPr_cnt++;
    }

    #pragma omp critical
    {
      BBPr_bigcyl[i] = BBPr_cnt/bigcylpts.size();     // Ratio of BBPr points in the neighborhood
    }
  }

  // Step 2 - Next we have to calculate he mean BBPr value for points in the neighborhood object for each focal point
  // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    double sum_of_elements = 0;                       // sum the elements in the neighborhood

    // Step 2.a Sphere neighborhood
    // ----------------------------

    std::vector<PointXYZ> sphpts;
    Sphere sphere(X[i], Y[i], Z[i], neigh_radii[0]);
    qtree.lookup(sphere, sphpts);

    sum_of_elements = 0;
    for (unsigned int j = 0; j < sphpts.size(); j++)
    {
      sum_of_elements += BBPr_sph[sphpts[j].id];
    }

    #pragma omp critical
    {
      meanBBPr_sph[i] = sum_of_elements/ptDen_sph[i];   // calculate the mean
    }

    // Step 2.b Small cylinder neighborhood
    // ------------------------------------

    std::vector<Point*> smcylpts;
    Circle smcircle(X[i], Y[i], neigh_radii[1]);
    qtree.lookup(smcircle, smcylpts);

    sum_of_elements = 0;
    double ptZ = Z[i];                                // the height of he focal point (lower end of the small cylinder)
    for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
    {
      if (Z[smcylpts[j]->id]>=ptZ)
        sum_of_elements += BBPr_smcyl[smcylpts[j]->id];
    }

    #pragma omp critical
    {
      meanBBPr_smcyl[i] = sum_of_elements/ptDen_smcyl[i]; // calculate the mean
    }

    // Step 2.c Big cylinder neighborhood
    // ----------------------------------

    std::vector<Point*> bigcylpts;
    Circle bigcircle(X[i], Y[i], neigh_radii[2]);
    qtree.lookup(bigcircle, bigcylpts);

    sum_of_elements = 0;
    for (unsigned int j = 0 ; j < bigcylpts.size() ; j++)
    {
      sum_of_elements += BBPr_bigcyl[bigcylpts[j]->id];
    }

    #pragma omp critical
    {
      meanBBPr_bigcyl[i] = sum_of_elements/ptDen_bigcyl[i]; // calculate the mean
    }
  }

  // Step 3 - Finally classify each point based on point density requirements and mean BBPr values from on the lookup table
  // in Wing et al 2015 - Table 2 - pg. 172 - here, the values supplied/specified by user in BBPRthrsh_mat
  // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  IntegerVector output(npoints); // vector to store the snag (or tree) classification values

  for(unsigned int i = 0 ; i < npoints ; i++)
  {
    if (ptDen_sph[i] >= pt_den_req &&
        meanBBPr_sph[i] >= BBPRthrsh_mat(0,0) &&
        ptDen_smcyl[i] >= pt_den_req &&
        meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,0) &&
        ptDen_bigcyl[i] >= pt_den_req &&
        meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,0))
    {
      output[i] = 1;  // General snag class
    }
    else if (ptDen_sph[i] >= 2 &&
             ptDen_sph[i] <= pt_den_req &&
             meanBBPr_sph[i] >= BBPRthrsh_mat(0,1) &&
             ptDen_smcyl[i] >= 2 &&
             ptDen_smcyl[i] <= pt_den_req &&
             meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,1) &&
             ptDen_bigcyl[i] >= 2 &&
             ptDen_bigcyl[i] <= pt_den_req &&
             meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,1))
    {
      output[i] = 2; // Small snag class
    }
    else if (ptDen_sph[i] >= pt_den_req &&
             meanBBPr_sph[i] >= BBPRthrsh_mat(0,2) &&
             ptDen_smcyl[i] >= pt_den_req &&
             meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,2) &&
             ptDen_bigcyl[i] >= pt_den_req*7 &&
             meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,2))
    {
      output[i] = 3; // Live crown edge snag class
    }
    else if (ptDen_sph[i] >= pt_den_req &&
             meanBBPr_sph[i] >= BBPRthrsh_mat(0,3) &&
             ptDen_smcyl[i] >= pt_den_req &&
             meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,3) &&
             ptDen_bigcyl[i] >= pt_den_req*15 &&
             meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,3))
    {
      output[i] = 4; // High canopy cover snag class
    }
    else
    {
      output[i] = 0; // Remaining points assigned to live tree class
    }
  }

  return(output);
}

IntegerVector LAS::segment_trees(double dt1, double dt2, double Zu, double R, double th_tree, double radius)
{
  double xmin = min(X);
  double ymin = min(Y);

  unsigned int ni = npoints;            // Number of points
  unsigned int n  = ni;                 // Number of remaining points
  unsigned int k  = 1;                  // Current tree ID

  // The ID of each point (returned object)
  IntegerVector idtree(ni);
  std::fill(idtree.begin(), idtree.end(), NA_INTEGER);

  // Square distance to speed up computation (dont need sqrt)
  radius = radius * radius;
  dt1 = dt1 * dt1;
  dt2 = dt2 * dt2;

  /* =====================
  * LI ET AL ALGORITHHM *
  ======================*/

  // Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A New Method for Segmenting Individual
  // Trees from the Lidar Point Cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75–84.
  // https://doi.org/10.14358/PERS.78.1.75

  // Find if a point is a local maxima within an R windows
  LogicalVector is_lm;
  if (R > 0)
  {
    filter_local_maxima(NumericVector::create(R), 0, true);
    is_lm = Rcpp::wrap(filter);
  }
  else
  {
    is_lm = LogicalVector(ni);
    std::fill(is_lm.begin(), is_lm.end(), true);
  }

  // A progress bar and abort options
  Progress p(ni, "Tree segmentation: ");

  // U the points to be segmented (see Li et al. page 78)
  std::vector<PointXYZ*> U(ni);
  for (unsigned int i = 0 ; i < ni ; ++i)
    U[i] = new PointXYZ(X[i], Y[i], Z[i], i);

  // N and P groups (see Li et al. page 78)
  std::vector<PointXYZ*> P,N;
  P.reserve(100);
  N.reserve(100);

  // A dummy point out of the dataset (see Li et al. page 79)
  PointXYZ* dummy = new PointXYZ(xmin-100,ymin-100,0,-1);

  // Z-sort the point cloud U
  std::sort(U.begin(), U.end(), ZSort<PointXYZ>());

  while(n > 0)
  {
    PointXYZ* u = U[0];
    std::vector<bool> inN(n);

    // Stop the algo is the highest point u, which is the target tree top, is below a threshold
    // Addition from original algo to limit over segmentaton
    if (u->z < th_tree)
    {
      p.update(ni);
    }
    else
    {
      if (p.check_interrupt())
      {
        for (unsigned int i = 0 ; i < U.size() ; i++) delete U[i]; // # nocov
        delete dummy; // # nocov
        p.exit(); // # nocov
      }

      p.update(ni-n);

      // Initial step no point in P or N
      P.clear();
      N.clear();

      // element 0 is the current highest point and is in P (target tree)
      P.push_back(u);
      idtree[u->id] = k;

      // Add the dummy point in N
      N.push_back(dummy);

      // Compute the distance between the current point u and all the &other points of U
      // This is not in the original algo. This is an optimisation to reduce the computation
      // time (see line 136).
      std::vector<double> d = sqdistance(U, *u);

      // Loop over each point of U (but the global maximum that is alredy in P)
      for (unsigned int i = 1 ; i < n ; ++i)
      {
        u = U[i];

        // If d > radius this point u is far and thus it is not in the current segmented tree
        // We don't need to apply the li et al. rules. This speed up a lot the computations
        if(d[i] > radius)
        {
          inN[i] = true;
        }
        // If d <= radius classify the point u based on Li et al. rules
        else
        {
          std::vector<double> dP = sqdistance(P, *u);
          std::vector<double> dN = sqdistance(N, *u);

          double dmin1 = *std::min_element(dP.begin(), dP.end());
          double dmin2 = *std::min_element(dN.begin(), dN.end());
          double dt    = (u->z > Zu) ? dt2 : dt1;

          if(is_lm[u->id]) // if u is a local maximum
          {
            if (dmin1 > dt || (dmin1 < dt && dmin1 > dmin2))
            {
              inN[i] = true;
              N.push_back(u);
            }
            else
            {
              P.push_back(u);
              idtree[u->id] = k;
            }
          }
          else // if u is not a local maximum
          {
            if (dmin1 <= dmin2)
            {
              P.push_back(u);
              idtree[u->id] = k;
            }
            else
            {
              inN[i] = true;
              N.push_back(u);
            }
          }
        }
      }
    }

    // Keep the point in N and redo the loop with remining points
    std::vector<PointXYZ*> temp;
    temp.reserve(N.size()-1);

    for (unsigned int i = 0 ; i < n ; i++)
    {
      if(inN[i])
        temp.push_back(U[i]);
      else
        delete U[i];
    }

    U.swap(temp);
    n = U.size();
    k++;                        // Increase current tree id
  }

  delete dummy;

  return idtree;
}

NumericVector LAS::rasterize(S4 layout, double subcircle, int method)
{
  S4 extent   = layout.slot("extent");
  int ncols   = layout.slot("ncols");
  int nrows   = layout.slot("nrows");
  double xmin = extent.slot("xmin");
  double xmax = extent.slot("xmax");
  double ymin = extent.slot("ymin");
  double ymax = extent.slot("ymax");
  double xres = (xmax - xmin) / ncols;
  double yres = (ymax - ymin) / nrows;

  NumericVector raster(ncols*nrows);
  std::fill(raster.begin(), raster.end(), NA_REAL);

  double (*f)(double x, double y);
  switch(method)
  {
  case 1: f = &LAS::rmax; break;
  case 2: f = &LAS::rmin; break;
  case 3: f = &LAS::rcount; break;
  default: Rcpp::stop("C++ unexpected internal error in 'rasterize': invalid method."); break; // # nocov;
  }

  if (subcircle > 0)
  {
    double cs[8] = {cos(0.0), cos(2*M_PI/8), cos(4*M_PI/8), cos(6*M_PI/8), cos(M_PI), cos(10*M_PI/8), cos(12*M_PI/8), cos(14*M_PI/8)};
    double ss[8] = {sin(0.0), sin(2*M_PI/8), sin(4*M_PI/8), sin(6*M_PI/8), sin(M_PI), sin(10*M_PI/8), sin(12*M_PI/8), sin(14*M_PI/8)};

    for (unsigned int i = 0 ; i < npoints ; i++)
    {
      double z = Z[i];

      for (unsigned int j = 0 ; j < 8 ; j++)
      {
        double x = X[i] + subcircle * cs[j];
        double y = Y[i] + subcircle * ss[j];

        int col = std::floor((x - xmin) / xres);
        int row = std::floor((ymax - y) / yres);
        if (y == ymin) row = nrows-1;
        if (x == xmax) col = ncols-1;

        if (row < 0 || row >= nrows || col < 0 || col >= ncols)
          Rcpp::stop("C++ unexpected internal error in 'rasterize': point out of raster."); // # nocov

        int cell = row * ncols + col;
        raster(cell) = f(raster(cell), z);

        // This is a hack for R 4.0.0 with alternative compiler toolchain (gcc8 32 bits)
        // I'm not able to understant why adding a print line fixes the problem
        // and I don't even know what is the problem.
        #ifdef _WIN32
        #ifdef __MINGW32__
        #ifdef __GNUC__
        #if __GNUC__ >= 8
                if (cell == raster.size() + 1) Rprintf("x = %lf, y = %lf\n", x, y);
        #endif
        #endif
        #endif
        #endif
      }
    }
  }
  else
  {
    for (int i = 0 ; i < X.length() ; i++)
    {
      double x = X[i];
      double y = Y[i];
      double z = Z[i];

      int col = std::floor((x - xmin) / xres);
      int row = std::floor((ymax - y) / yres);
      if (y == ymin) row = nrows-1;
      if (x == xmax) col = ncols-1;

      if (row < 0 || row >= nrows || col < 0 || col >= ncols)
        Rcpp::stop("C++ unexpected internal error in 'rasterize': point out of raster."); // # nocov

      int cell = row * ncols + col;
      raster(cell) = f(raster(cell), z);
    }
  }

  return raster;
}

List LAS::knn_metrics(unsigned int k, DataFrame data, DataFrame sub, SEXP call, SEXP env)
{
  int nprocessed = std::count(filter.begin(), filter.end(), true);
  int j = 0;
  List output(nprocessed);
  SpatialIndex tree(X,Y,Z,filter);
  Progress pb(npoints, "Metrics computation: ");
  bool abort = false;
  int pOutError = 0;

  for(unsigned int i = 0 ; i < npoints ; ++i) {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();
    if (!filter[i]) continue;


    PointXYZ p(X[i], Y[i], Z[i]);
    std::vector<PointXYZ> pts;
    tree.knn(p, k, pts);

    Rcpp::DataFrame::iterator it2 = sub.begin();
    for (Rcpp::DataFrame::iterator it1 = data.begin() ; it1 != data.end() ; ++it1) {
      switch( TYPEOF(*it1) ) {
        case REALSXP: {
          Rcpp::NumericVector tmp1 = Rcpp::as<Rcpp::NumericVector>(*it1);
          Rcpp::NumericVector tmp2 = Rcpp::as<Rcpp::NumericVector>(*it2);
          for(unsigned int i = 0 ; i < k ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        case INTSXP: {
          Rcpp::IntegerVector tmp1 = Rcpp::as<Rcpp::IntegerVector>(*it1);
          Rcpp::IntegerVector tmp2 = Rcpp::as<Rcpp::IntegerVector>(*it2);
          for(unsigned int i = 0 ; i < k ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        case LGLSXP: {
          Rcpp::LogicalVector tmp1 = Rcpp::as<Rcpp::LogicalVector>(*it1);
          Rcpp::LogicalVector tmp2 = Rcpp::as<Rcpp::LogicalVector>(*it2);
          for(unsigned int i = 0 ; i < k ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        default: {
          Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs");
        }
      }
      ++it2;
    }

    output[j] = R_tryEvalSilent(call, env, &pOutError);

    if (pOutError == 1)
      throw Rcpp::exception(R_curErrorBuf(), false);

    j++;
  }

  return output;
}
