#include "LAS.h"
#include "Progress.h"
#include "myomp.h"
#include "SpatialIndex.h"
#include <limits>
#include <boost/geometry.hpp>

using namespace lidR;

LAS::LAS(S4 las, int ncpu)
{
  Rcpp::List index = las.slot("index");
  this->sensor = index["sensor"];

  this->las = las;

  DataFrame data = as<DataFrame>(las.slot("data"));
  this->X = data["X"];
  this->Y = data["Y"];
  this->Z = data["Z"];

  if (data.containsElementNamed("Intensity"))
    this->I = data["Intensity"];

  if (data.containsElementNamed("gpstime"))
    this->T = data["gpstime"];

  this->npoints = X.size();
  this->ncpu = ncpu;
  this->filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), false);
  this->skip.resize(npoints);
  std::fill(skip.begin(), skip.end(), false);
}

void LAS::new_filter(LogicalVector b)
{
  if (b.size() == 1)
    std::fill(skip.begin(), skip.end(), b[0]);
  else if (b.size() == (int)npoints)
    this->skip = Rcpp::as< std::vector<bool> >(b);
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

  SpatialIndex tree(las);

  Progress pb(npoints, "Point cloud smoothing: ");

  bool abort = false;

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    std::vector<PointXYZ> pts;

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
        double dx = X[i] - pts[j].x;
        double dy = Y[i] - pts[j].y;
        w = 1/twosquaresigmapi * std::exp(-(dx*dx + dy*dy)/twosquaresigma);
      }

      ztot += w*pts[j].z;
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

  SpatialIndex tree(las, skip);

  Progress p(2*npoints, "Morphological filter: ");

  // Dilate
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    p.check_abort();
    p.update(i);
    if (!skip[i]) continue;

    std::vector<PointXYZ> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double min_pt(std::numeric_limits<double>::max());

    for(unsigned  int j = 0 ; j < pts.size() ; j++)
    {
      double z = pts[j].z;

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
    if (!skip[i]) continue;

    std::vector<PointXYZ> pts;
    Rectangle rect(X[i]-half_res, X[i]+half_res,Y[i]-half_res, Y[i]+half_res);
    tree.lookup(rect, pts);

    double max_pt(std::numeric_limits<double>::min());

    for(unsigned int j = 0 ; j < pts.size() ; j++)
    {
      double z = Z_temp[pts[j].id];

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

  double i;

  // Compute the median sensor elevation then average range for this sensor
  // elevation. This gives a rough idea of the expected range and allows for
  // detecting failure and bad computations
  double median_z_sensor = Rcpp::median(z);
  double R_control = mean(median_z_sensor - Z);

  IntegerVector Inorm(X.size());

  Progress pbar(npoints, "Range computation");

  // Loop on each point
  for (unsigned int k = 0 ; k < npoints ; k++)
  {
    pbar.increment();
    pbar.check_abort();

    double R = range(x, y, z, t, k, R_control);

    i = I[k] * std::pow((R/Rs),f);

    if (i > 65535)
    {
      Rf_warningcall(R_NilValue, "Normalized intensity does not fit in 16 bits. Value clamped to 2^16.");
      i = 65535;
    }

    Inorm[k]  = i;
  }

  I = Inorm;

  return;
}

double LAS::range(NumericVector &x, NumericVector &y , NumericVector &z, NumericVector &t,  int k, double R_control)
{
  NumericVector::iterator it;
  double dx, dy, dz, r, R;
  int j = 0;

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
  // If t1-t0 is too big it is two differents flightlines. We must hold this case by chosing
  // if we use the previous point or this one.
  else if (std::abs(*it - *(it-1)) > 30)
  {
    // If t is closer to the previous one
    if (std::abs(T[k] - *(it-1)) < std::abs(T[k] - *(it+1)))
      j = it - t.begin() - 1;
    else
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

  if (sensor != TLS && R > 3 * R_control)
  {
    Rprintf("An high range R has been computed relatively to the expected average range Rm = %.0lf\n", R_control);
    Rprintf("Point number %d at (x,y,z,t) = (%.2lf, %.2lf, %.2lf, %.2lf)\n", k+1, X[k], Y[k], Z[k], T[k]);
    Rprintf("Matched with sensor between (%.2lf, %.2lf, %.2lf, %.2lf) and (%.2lf, %.2lf, %.2lf, %.2lf)\n", x[j-1], y[j-1], z[j-1], t[j-1], x[j], y[j], z[j], t[j]);
    Rprintf("The range computed was R = %.2lf\n", R, dx, dy, dz, t[j]);
    Rprintf("Check the correctness of the sensor positions and the correctness of the gpstime either in the point cloud or in the sensor positions.\n");
    throw Rcpp::exception("Unrealistic range: see message above", false);
  }

  return R;
}

NumericVector LAS::compute_range(DataFrame flightlines)
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

  NumericVector R(npoints);

  Progress pbar(npoints, "Range computation");

  // Loop on each point
  for (unsigned int k = 0 ; k < npoints ; k++)
  {
    pbar.increment();
    pbar.check_abort();
    R[k] = range(x, y, z, t, k, R_control);
  }

  return R;
}

void LAS::filter_local_maxima(NumericVector ws, double min_height, bool circular)
{
  bool abort = false;
  bool vws = ws.length() > 1;

  SpatialIndex tree(las);
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
    std::vector<PointXYZ> pts;
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

    // Initialize the highest point using the central point
    PointXYZ p(X[i], Y[i], Z[i], i);
    double zmax = Z[i];
    bool is_lm = true;

    // Search if one is higher
    for (auto pt : pts)
    {
      double z = pt.z;

      // Found one higher, it is not a LM
      if(z > zmax)
      {
        is_lm = false;
        break;
      }

      // Found one equal. If this one was already tagged LM we can't have two lm
      // The first tagged has the precedence
      if (z == zmax && filter[pt.id])
      {
        is_lm = false;
        break;
      }
    }

    #pragma omp critical
    {
      filter[i] = is_lm;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return;
}

void LAS::filter_local_maxima(NumericVector ws)
{
  bool abort = false;
  int mode;
  double radius = 0;
  double hwidth = 0;
  double hheight = 0;
  double orientation = 0;

  if (ws.length() == 1)
  {
    mode = 1; // circular windows
    radius = ws[0]/2;
  }
  else if (ws.length() == 2)
  {
    mode = 2;  // rectangular windows
    hwidth = ws[0]/2;
    hheight = ws[1]/2;
  }
  else if (ws.length() == 3)
  {
    mode = 3;  // rectangular oriented windows
    hwidth = ws[0]/2;
    hheight = ws[1]/2;
    orientation = ws[2];
  }
  else
    Rcpp::stop("C++ unexpected internal error in 'filter_local_maxima': invalid windows."); // # nocov

  SpatialIndex tree(las, skip);
  Progress pb(npoints, "Local maximum filter: ");

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();
    if (!skip[i]) continue;

    // Get the points within a windows centred on the current point
    std::vector<PointXYZ> pts;
    switch(mode)
    {
      case 1: {
        Circle circ(X[i], Y[i], radius);
        tree.lookup(circ, pts);
        break;
      }
      case 2: {
        Rectangle rect(X[i] - hwidth, X[i] + hwidth, Y[i] - hheight, Y[i] + hheight);
        tree.lookup(rect, pts);
        break;
      }
      case 3: {
        double hwidth = ws[0]/2;
        double hheight = ws[1]/2;
        double orientation = ws[2];
        OrientedRectangle orect(X[i] - hwidth, X[i] + hwidth, Y[i] - hheight, Y[i] + hheight, orientation);
        tree.lookup(orect, pts);
        break;
      }
    }

    // Get the highest Z in the windows
    double Zmax = std::numeric_limits<double>::min();
    PointXYZ p = pts[0];
    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      if(pts[j].z > Zmax)
      {
        p = pts[j];
        Zmax = Z[p.id];
      }
    }

    // The central pixel is the highest, it is a LM
    #pragma omp critical
    {
      if (Z[i] == Zmax && X[i] == p.x && Y[i] == p.y)
        filter[i] = true;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return;
}

void LAS::filter_with_grid(List layout, bool max)
{
  int ncols   = layout["ncol"];
  int nrows   = layout["nrow"];
  double xmin = layout["xmin"];
  double xmax = layout["xmax"];
  double ymin = layout["ymin"];
  double ymax = layout["ymax"];
  double xres = (xmax - xmin) / ncols;
  double yres = (ymax - ymin) / nrows;
  int limit = (max) ? std::numeric_limits<int>::min() : std::numeric_limits<int>::max();

  std::vector<int> output(ncols*nrows);
  std::fill(output.begin(), output.end(), limit);

  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (skip[i]) continue;

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

    if (output[cell] == limit)
    {
      output[cell] = i;
    }
    else
    {
      double zref = Z[output[cell]];
      if (max) {
        if (zref < z) output[cell] = i;
      } else {
        if (zref > z) output[cell] = i;
      }
    }
  }

  for (unsigned int i = 0 ; i < output.size() ; i++)
  {
    if (output[i] != limit)
      filter[output[i]] = true;
  }

  return;
}

IntegerVector LAS::find_polygon_ids(CharacterVector wkts)
{
  typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> Point;
  typedef boost::geometry::model::polygon<Point> Polygon;
  typedef boost::geometry::model::multi_polygon<Polygon> MultiPolygon;
  typedef boost::geometry::model::box<Point> Bbox;

  SpatialIndex tree(las);
  IntegerVector poly_id(X.size());
  std::fill(poly_id.begin(), poly_id.end(), NA_INTEGER);

  for (unsigned int j = 0 ; j < wkts.size() ; j++)
  {
    bool is_in_polygon = false;
    std::string wkt = as<std::string>(wkts[j]);

    if (wkt.find("MULTIPOLYGON") != std::string::npos)
    {
      Point p;
      Bbox bbox;
      MultiPolygon polygons;

      boost::geometry::read_wkt(wkt, polygons);
      boost::geometry::envelope(polygons, bbox);

      double min_x = bbox.min_corner().get<0>();
      double min_y = bbox.min_corner().get<1>();
      double max_x = bbox.max_corner().get<0>();
      double max_y = bbox.max_corner().get<1>();

      lidR::Rectangle rect(min_x, max_x, min_y, max_y);
      std::vector<PointXYZ> pts;
      tree.lookup(rect, pts);

      for(unsigned int i = 0 ; i < pts.size() ; i++)
      {
        p.set<0>(pts[i].x);
        p.set<1>(pts[i].y);
        if (boost::geometry::covered_by(p, polygons))
          poly_id[pts[i].id] = j;
      }
    }
    else if (wkt.find("POLYGON") != std::string::npos)
    {
      Point p;
      Bbox bbox;
      Polygon polygon;

      boost::geometry::read_wkt(wkt, polygon);
      boost::geometry::envelope(polygon, bbox);

      double min_x = bbox.min_corner().get<0>();
      double min_y = bbox.min_corner().get<1>();
      double max_x = bbox.max_corner().get<0>();
      double max_y = bbox.max_corner().get<1>();

      lidR::Rectangle rect(min_x, max_x, min_y, max_y);
      std::vector<PointXYZ> pts;
      tree.lookup(rect, pts);

      for(unsigned int i = 0 ; i < pts.size() ; i++)
      {
        p.set<0>(pts[i].x);
        p.set<1>(pts[i].y);
        if (boost::geometry::covered_by(p, polygon))
          poly_id[pts[i].id] = j;
      }
    }
    else
      Rcpp::stop("Unexpected error in point_in_polygon: WKT is not a POLYGON or MULTIPOLYGON"); // # nocov

  }

  return poly_id;
}

void LAS::filter_shape(int method, NumericVector th, int k)
{
  Progress pb(npoints, "Eigenvalues computation: ");

  bool abort = false;

  SpatialIndex qtree(las, skip);

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
    if (!skip[i]) continue;

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
      if (!skip[j]) continue;
      skip[j] = (oldZ[j] - Z[j]) < th[i];
    }
  }

  filter = skip;

  return;
}

void LAS::filter_isolated_voxel(double res, unsigned int isolated)
{
  double xmin = min(X);
  double ymin = min(Y);
  double zmin = min(Z);
  double xmax = max(X);
  double ymax = max(Y);
  double zmax = max(Z);

  unsigned int width = std::floor((xmax - xmin) / res);
  unsigned int height = std::floor((ymax - ymin) / res);
  //unsigned int depth = std::floor((zmax - zmin) / res);

  // Stores for a given voxel the number of point in its 27 voxels neighbourhood
  std::unordered_map<unsigned int, unsigned int> dynamic_registry;

  for (unsigned int n = 0 ; n < npoints ; n++)
  {
    int nx = std::floor((X[n] - xmin) / res);
    int ny = std::floor((Y[n] - ymin) / res);
    int nz = std::floor((Z[n] - zmin) / res);

    // Add one in the 27 neighbouring voxel of this point
    for (int i : {-1,0,1})
    {
      for (int j : {-1,0,1})
      {
        for (int k : {-1,0,1})
        {
          if (!(i == 0 && j == 0 && k == 0))
          {
            unsigned int key = (nx+i) + (ny+j)*width + (nz+k)*width*height;
            dynamic_registry.insert({key, 0});
            dynamic_registry[key]++;
          }
        }
      }
    }
  }

  // Loop again through each point.
  // Check if the number of points in its neighbourhood is above the threshold
  for (unsigned int n = 0 ; n < npoints ; n++)
  {
    int nx = std::floor((X[n] - xmin) / res);
    int ny = std::floor((Y[n] - ymin) / res);
    int nz = std::floor((Z[n] - zmin) / res);
    unsigned int key = nx + ny*width + nz*width*height;
    filter[n] = dynamic_registry[key] <= isolated;
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

  SpatialIndex qtree(las);                  // the SpatialIndex for the las object

  // Step 1 - First we have to build neighborhood objects (sphere, small and large cylinders) around each focal point and get
  // the BBPr counts, then we have to calculate the actual ratio of BBPr to neighborhood points for each focal point
  // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    double BBPr_cnt = 0;                              // the count of BBPr points (based on thresholds) in the neighborhood

    // Step 1.a Sphere neighborhood
    // ----------------------------

    std::vector<PointXYZ> sphpts;                    // creation of an STL container of points for the sphere neighborhood object
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

    std::vector<PointXYZ> smcylpts;                  // creation of an STL container of points for the small cylinder neighborhood object
    Circle smcircle(X[i], Y[i], neigh_radii[1]);      // creation of a small cylinder object
    qtree.lookup(smcircle, smcylpts);                 // lookup the points in the small cylinder neighborhood

    BBPr_cnt = 0;
    double ptZ = Z[i];                                // the height of the focal point (lower end of the small cylinder)
    for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
    {
      if (smcylpts[j].z >= ptZ)
      {
        ptDen_smcyl[i]++;
        if (I[smcylpts[j].id] <= low_int_thrsh || I[smcylpts[j].id] >= uppr_int_thrsh)
          BBPr_cnt++;
      }
    }

    #pragma omp critical
    {
      BBPr_smcyl[i] = BBPr_cnt/ptDen_smcyl[i];        // Ratio of BBPr points in the neighborhood
    }

    // Step 1.c Big cylinder neighborhood
    // ----------------------------------

    std::vector<PointXYZ> bigcylpts;                    // creation of an STL container of points for the big cylinder neighborhood object
    Circle bigcircle(X[i], Y[i], neigh_radii[2]);     // creation of a big cylinder object
    qtree.lookup(bigcircle, bigcylpts);               // lookup the points in the big cylinder neighborhood
    ptDen_bigcyl[i] = bigcylpts.size();               // get the point density in the big cylinder neighborhood

    BBPr_cnt = 0;
    for (unsigned int j = 0; j < bigcylpts.size(); j++)
    {
      if (I[bigcylpts[j].id] <= low_int_thrsh || I[bigcylpts[j].id] >= uppr_int_thrsh)
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

    std::vector<PointXYZ> smcylpts;
    Circle smcircle(X[i], Y[i], neigh_radii[1]);
    qtree.lookup(smcircle, smcylpts);

    sum_of_elements = 0;
    double ptZ = Z[i];                                // the height of he focal point (lower end of the small cylinder)
    for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
    {
      if (smcylpts[j].z >= ptZ)
        sum_of_elements += BBPr_smcyl[smcylpts[j].id];
    }

    #pragma omp critical
    {
      meanBBPr_smcyl[i] = sum_of_elements/ptDen_smcyl[i]; // calculate the mean
    }

    // Step 2.c Big cylinder neighborhood
    // ----------------------------------

    std::vector<PointXYZ> bigcylpts;
    Circle bigcircle(X[i], Y[i], neigh_radii[2]);
    qtree.lookup(bigcircle, bigcylpts);

    sum_of_elements = 0;
    for (unsigned int j = 0 ; j < bigcylpts.size() ; j++)
    {
      sum_of_elements += BBPr_bigcyl[bigcylpts[j].id];
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

NumericVector LAS::rasterize(List layout, double subcircle, int method)
{
  int ncols   = layout["ncol"];
  int nrows   = layout["nrow"];
  double xmin = layout["xmin"];
  double xmax = layout["xmax"];
  double ymin = layout["ymin"];
  double ymax = layout["ymax"];
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

        if (!(row < 0 || row >= nrows || col < 0 || col >= ncols))
        {
          int cell = row * ncols + col;
          raster(cell) = f(raster(cell), z);

          // This is a hack for R 4.0.0 with alternative compiler toolchain (gcc8 32 bits)
          // I'm not able to understand why adding a print line fixes the problem
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

      if (!(row < 0 || row >= nrows || col < 0 || col >= ncols))
      {
        int cell = row * ncols + col;
        raster(cell) = f(raster(cell), z);
      }
    }
  }

  return raster;
}

List LAS::point_metrics(unsigned int k, double r, DataFrame data, int nalloc, SEXP call, SEXP env)
{
  // @k is the k of knn
  // @r is the radius
  // @data contains all the attributes of the LAS object so we are capable
  //       of copying the value in temporary memory
  // @n is the length of the vectors initially allocated to store the neighborhood
  // @call is the user-defined expression to apply on each neighborhood
  // @env is the environnement where Rf_eval eval the call

  // Are we searching the k nearest neiborhood or a sphere neighborhood or both?
  int mode = 0;
  if (k == 0 && r > 0)
    mode = 1;
  else if (k > 0 && r == 0)
    mode = 0;
  else if (k > 0 && r > 0)
    mode = 2;
  else
    Rcpp::stop("Internal error: invalid argument k or r");

  // Do we need to manage dynamic memory? Yes if not pure knn
  bool dynamic_memory_realloc = mode > 0;

  // Create an Rcpp object to handle the SEXP easily otherwise we have to use R's C API...
  Rcpp::Environment callenv = as<Rcpp::Environment>(env);

  // Retrieve the names of the attributes
  std::vector<std::string> names = as<std::vector<std::string> >(data.names());

  // Need some iterator to loop on the List columns
  Rcpp::List::iterator it1;
  Rcpp::List::iterator it2;
  std::vector<std::string>::iterator it3;

  // Need a physical List to handle a reference to the data of the environement 'env'.
  // Not mandatory stricly speaking but easier to handle this stuff with Rcpp than R's C API
  List proxy;

  // We populate the environement by creating new vector of size nalloc binded to
  // the original names in the LAS object + a shallow copy in the List 'proxy'.
  it3 = names.begin();
  for (it1 = data.begin() ; it1 != data.end() ; ++it1)
  {
    switch( TYPEOF(*it1) )
    {
      case REALSXP: {
        Rcpp::NumericVector tmp(nalloc);
        proxy.push_back(tmp);
        callenv.assign(*it3, tmp);
        break;
      }
      case INTSXP: {
        Rcpp::IntegerVector tmp(nalloc);
        proxy.push_back(tmp);
        callenv.assign(*it3, tmp);
        break;
      }
      case LGLSXP: {
        Rcpp::LogicalVector tmp(nalloc);
        proxy.push_back(tmp);
        callenv.assign(*it3, tmp);
        break;
      }
      default: {
        Rcpp::stop("Incompatible type encountered: integer, double and bool are the only supported types.");
      }
    }

    ++it3;
  }

  // Number of points actually processed considering the filter.
  // The output is allocated using this number
  int nprocessed = std::count(skip.begin(), skip.end(), true);
  List output(nprocessed);

  // Current index in the output
  int j = 0;

  // Construction of a spatial index to make the queries
  SpatialIndex tree(las, skip);
  Progress pb(npoints, "Metrics computation: ");

  // Error handling variables
  bool abort = false;
  int pOutError = 0;

  // This is the size of memory used to store the neighborhood
  unsigned int si = nalloc; // initially allocated
  unsigned int sc = si;     // current
  unsigned int sn = si;     // new

  if (!dynamic_memory_realloc && si != k) Rcpp::stop("Internal error: k elements should have been allocated.");

  //Rprintf("Memory allocated to store the neighborhood: %d\n", si);

  // Loop through each points
  for(unsigned int i = 0 ; i < npoints ; ++i)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();
    if (!skip[i]) continue;

    std::vector<PointXYZ> pts;

    if (mode == 0)
    {
      // Query the knn neighborhood
      PointXYZ p(X[i], Y[i], Z[i]);
      tree.knn(p, k, pts);

      // No need to reallocate the memory because it is always of size k
    }
    else
    {
      if (mode == 1)
      {
        // Query the sphere neighborhood
        Sphere sp(X[i], Y[i], Z[i], r);
        tree.lookup(sp, pts);
      }
      else
      {
        // Query the knn + sphere limit
        PointXYZ p(X[i], Y[i], Z[i]);
        tree.knn(p, k, r, pts);
      }

      // This is the new size of the memory used to store the neighborhood
      sn = pts.size();

      // If we have found more points in the neighborhood than we have memory allocated: we need
      // to resize the memory. We resize x2 so we are sure that will occur only one or two times.
      if (sn > si)
      {
        si = (sn < 2*si) ? 2*si : sn;
        //Rprintf("Realloc from %d to %d at point %d because neigborhood of size %d\n", nalloc, si, i, sn);
        nalloc = si;
        sc = sn;
        proxy = List::create();

        it3 = names.begin();
        for (it1 = data.begin() ; it1 != data.end() ; ++it1)
        {
          switch( TYPEOF(*it1) )
          {
            case REALSXP: {
              Rcpp::NumericVector tmp(nalloc);
              proxy.push_back(tmp);
              callenv.assign(*it3, tmp);
              SETLENGTH(wrap(tmp), sc);
              break;
            }
            case INTSXP: {
              Rcpp::IntegerVector tmp(nalloc);
              proxy.push_back(tmp);
              callenv.assign(*it3, tmp);
              SETLENGTH(wrap(tmp), sc);
              break;
            }
            case LGLSXP: {
              Rcpp::LogicalVector tmp(nalloc);
              proxy.push_back(tmp);
              callenv.assign(*it3, tmp);
              SETLENGTH(wrap(tmp), sc);
              break;
            }
            default: {
              Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
            }
          }

          ++it3;
        }

      }
      // If we have found less points in the neighborhood than we have memory allocated: resize memory
      else
      {
        sc = sn;
        for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
          SETLENGTH(*it1, sc);
      }
    }


    // At this stage the environnment env should contains vectors named like into the LAS
    // object and these vector are longer or equal to the number of points in the neihborhood.
    // But at the R level they are of the good lenght because we used SETLENGTH.
    it2 = proxy.begin();
    for (it1 = data.begin() ; it1 != data.end() ; ++it1)
    {
      switch( TYPEOF(*it1) )
      {
        case REALSXP: {
          Rcpp::NumericVector tmp1 = Rcpp::as<Rcpp::NumericVector>(*it1);
          Rcpp::NumericVector tmp2 = Rcpp::as<Rcpp::NumericVector>(*it2);
          for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        case INTSXP: {
          Rcpp::IntegerVector tmp1 = Rcpp::as<Rcpp::IntegerVector>(*it1);
          Rcpp::IntegerVector tmp2 = Rcpp::as<Rcpp::IntegerVector>(*it2);
          for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        case LGLSXP: {
          Rcpp::LogicalVector tmp1 = Rcpp::as<Rcpp::LogicalVector>(*it1);
          Rcpp::LogicalVector tmp2 = Rcpp::as<Rcpp::LogicalVector>(*it2);
          for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
          break;
        }
        default: {
          Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
        }
      }

      ++it2; ++it3;
    }

    output[j] = R_tryEvalSilent(call, env, &pOutError);

    if (pOutError == 1)
    {
      // Restore the TRUELENGTH otherwise memory leak
      if (dynamic_memory_realloc)
      {
        for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
          SETLENGTH(*it1, si);
      }

      throw Rcpp::exception(R_curErrorBuf(), false);
    }

    j++;
  }

  // Restore the TRUELENGTH otherwise memory leak
  if (dynamic_memory_realloc)
  {
    for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
      SETLENGTH(*it1, si);
  }

  return output;
}

DataFrame LAS::eigen_decomposition(int k, double r)
{
  int n = std::count(skip.begin(), skip.end(), true);
  IntegerVector pointID(n);
  NumericVector eigen_largest(n);
  NumericVector eigen_medium(n);
  NumericVector eigen_smallest(n);

  bool abort = false;
  unsigned int j = 0;

  int mode = 0;
  if (k == 0 && r > 0)
    mode = 1;
  else if (k > 0 && r == 0)
    mode = 0;
  else if (k > 0 && r > 0)
    mode = 2;
  else
    Rcpp::stop("Internal error: invalid argument k or r");

  SpatialIndex index(las, skip);
  Progress pb(npoints, "Eigen decomposition: ");


  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();
    if (!skip[i]) continue;

    PointXYZ p(X[i], Y[i], Z[i]);

    std::vector<PointXYZ> pts;
    switch (mode)
    {
      case 0:
      {
        index.knn(p, k, pts);
        break;
      }

      case 1:
      {
        Sphere sp(p.x, p.y, p.z, r);
        index.lookup(sp, pts);
        break;
      }

      case 2:
      {
        index.knn(p, k, r, pts);
        break;
      }

      default:
      {
        Rcpp::stop("Internal error in LAS::eigen_decomposition: invalid search mode");
        break;
      }
    }

    arma::mat A(pts.size(),3);
    arma::mat coeff;  // Principle component matrix
    arma::mat score;
    arma::vec latent; // Eigenvalues in descending order

    for (unsigned int k = 0 ; k < pts.size() ; k++)
    {
      A(k,0) = pts[k].x;
      A(k,1) = pts[k].y;
      A(k,2) = pts[k].z;
    }

    arma::princomp(coeff, score, latent, A);

    #pragma omp critical
    {
      pointID[j] = i;
      eigen_largest[j] = latent[0];
      eigen_medium[j] = latent[1];
      eigen_smallest[j] = latent[2];
      j++;
    }
  }

  DataFrame out;
  out.push_back(pointID+1, "pointID");
  out.push_back(eigen_largest, "eigen_largest");
  out.push_back(eigen_medium, "eigen_medium");
  out.push_back(eigen_smallest, "eigen_smallest");
  return out;
}


NumericVector LAS::fast_knn_metrics(unsigned int k, IntegerVector metrics)
{
  Progress pb(npoints, "Metrics computation: ");

  bool abort = false;

  SpatialIndex tree(las);

  NumericVector out(npoints);

  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < npoints ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true; // No data race here because only thread 0 can actually write
    pb.increment();

    PointXYZ p(X[i], Y[i], Z[i]);

    std::vector<PointXYZ> pts;
    tree.knn(p, k, pts);

    double d = 0;
    double dmean = 0;
    for (unsigned int j = 1 ; j < pts.size() ; j++)
    {
      d = std::sqrt((p.x - pts[j].x)*(p.x - pts[j].x) + (p.y - pts[j].y)*(p.y - pts[j].y) + (p.z - pts[j].z)*(p.z - pts[j].z));
      dmean += d;
    }

    #pragma omp critical
    {
      out(i) = dmean/(double)(k-1);
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return out;
}

NumericVector LAS::interpolate_knnidw(NumericVector x, NumericVector y, int k, double p, double rmax)
{
  unsigned int n = x.length();
  NumericVector iZ(n, NA_REAL);

  SpatialIndex tree(las);
  Progress pb(n, "Inverse distance weighting: ");

  bool abort = false;

  #pragma omp parallel for num_threads(ncpu)
  for(unsigned int i = 0 ; i < n ; i++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    Point pt(x[i], y[i]);
    std::vector<PointXYZ> pts;
    tree.knn(pt, k, rmax, pts);

    double sum_zw = 0;
    double sum_w  = 0;

    for (unsigned int j = 0 ; j < pts.size() ; j++)
    {
      double dx = pts[j].x - x[i];
      double dy = pts[j].y - y[i];
      double d  = std::sqrt(dx*dx + dy*dy);
      double w;
      double z = Z[pts[j].id];

      if (d > 0)
      {
        w = 1/pow(d,p);
        sum_zw += z*w;
        sum_w  += w;
      }
      else
      {
        sum_zw = z;
        sum_w  = 1;
        break;
      }
    }

    #pragma omp critical
    {
      iZ(i) = sum_zw/sum_w;
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return iZ;
}

