#include "lidR/GridPartition.h"
#include "Progress.h"

#include <boost/polygon/voronoi.hpp>

using boost::polygon::voronoi_diagram;
using namespace Rcpp;
using namespace lidR;



// The point structure must be an integral number. Boost perform on integral number only.
struct point_int
{
  int x;
  int y;
  int id;
  point_int(int x_, int y_, int i) : x(x_), y(y_), id(i) {}
};

namespace boost
{
  namespace polygon
  {
    template <>
    struct geometry_concept<point_int>
    {
      typedef point_concept type;
    };

    template <>
    struct point_traits<point_int>
    {
      typedef double coordinate_type;

      static inline coordinate_type get(const point_int& point, orientation_2d orient)
      {
        return (orient == HORIZONTAL) ? point.x : point.y;
      }
    };
  }
}

// [[Rcpp::export(rng = false)]]
IntegerMatrix C_delaunay(DataFrame P, NumericVector scales, NumericVector offsets, double trim = 0)
{
  if (!P.containsElementNamed("X") || !P.containsElementNamed("Y"))
    throw Rcpp::exception("Internal error in C_delaunay: columns are not named XY.", false);

  if (scales.size() != 2 || offsets.size() != 2)
    throw Rcpp::exception("Internal error in C_delaunay: scales and/or offset are not of size 2.", false);

  if (scales[0] != scales[1])
    throw Rcpp::exception("Internal error in C_delaunay: cannot triangulate points with different xy scale factors.", false);

  // The point cloud
  NumericVector X = P["X"];
  NumericVector Y = P["Y"];

  if (X.size() < 3)
    throw Rcpp::exception("Internal error in C_delaunay: cannot triangulate less than 3 points.", false);

  bool partial_triangulation = trim != 0;
  double atrim = trim * trim;

  // Need scale and offset to convert to integral numbers
  double scale_x = scales[0];
  double scale_y = scales[1];
  double offset_x = offsets[0];
  double offset_y = offsets[1];

  // Convert to boost point concept.
  // Coordinates are unscaled unoffseted for integer-based computation
  double dx,dy;
  int ix,iy;
  std::vector<point_int> points;
  for (int i = 0 ; i < X.size() ; i++)
  {
    dx = (X[i]-offset_x)/scale_x;
    dy = (Y[i]-offset_y)/scale_y;
    ix = std::round(dx);
    iy = std::round(dy);

    if (std::abs(dx - ix) > 1e-5 || std::abs(dy - iy) > 1e-5)
      throw Rcpp::exception("Internal error in C_delaunay: xy coordinates were not converted to integer. Scale factors are likely to be invalid.", false);

    point_int p(ix, iy, i);
    points.push_back(p);
  }

  // Construction of the Voronoi Diagram (this step is fast).
  boost::polygon::voronoi_diagram<double> vd;
  construct_voronoi(points.begin(), points.end(), &vd);

  IntegerMatrix output(vd.num_vertices(), 3);
  std::fill(output.begin(), output.end(), IntegerVector::get_na() ) ;

  for (unsigned int i = 0 ; i < vd.num_vertices() ; ++i)
  {
    std::vector<point_int> triangle;
    const voronoi_diagram<double>::const_vertex_iterator it = vd.vertices().begin() + i;
    const voronoi_diagram<double>::vertex_type& vertex = *it;
    const voronoi_diagram<double>::edge_type* edge = vertex.incident_edge();

    do
    {
      const voronoi_diagram<double>::cell_type* cell = edge->cell();
      assert(cell->contains_point());

      triangle.push_back(points[cell->source_index()]);

      if (triangle.size() == 3)
      {
        if (partial_triangulation)
        {
          // Current triangle is ABC. Scale and offset back for double precision computation
          point_int &A_ = triangle[0];
          Point A(A_.x*scale_x+offset_x, A_.y*scale_y+offset_y, A_.id);
          point_int &B_ = triangle[1];
          Point B(B_.x*scale_y+offset_x, B_.y*scale_y+offset_y, B_.id);
          point_int &C_ = triangle[2];
          Point C(C_.x*scale_x+offset_x, C_.y*scale_y+offset_y, C_.id);

          // ABC is represented by vector u,v and w
          Point u(A.x - B.x, A.y - B.y);
          Point v(A.x - C.x, A.y - C.y);
          Point w(B.x - C.x, B.y - C.y);

          // Compute the AB, AC, BC edge comparable length
          double edge_AB = u.x * u.x + u.y * u.y;
          double edge_AC = v.x * v.x + v.y * v.y;
          double edge_BC = w.x * w.x + w.y * w.y;
          double edge_max = MAX(edge_AB, edge_AC, edge_BC);

          bool skip = (trim > 0) ? edge_max > atrim : edge_max < atrim;

          if (!skip)
          {
            output(i, 0) = triangle[0].id;
            output(i, 1) = triangle[1].id;
            output(i, 2) = triangle[2].id;
          }
        }
        else
        {
          output(i, 0) = triangle[0].id;
          output(i, 1) = triangle[1].id;
          output(i, 2) = triangle[2].id;
        }

        triangle.erase(triangle.begin() + 1);
      }

      edge = edge->rot_next();

    } while (edge != vertex.incident_edge());
  }

  return output + 1;
}

// [[Rcpp::export(rng = false)]]
NumericVector C_interpolate_delaunay(DataFrame P, DataFrame L, NumericVector scales, NumericVector offsets, double trim = 0, double min_normal_z = 0, int ncpu = 1)
{
  if (!P.containsElementNamed("X") || !P.containsElementNamed("Y") || !P.containsElementNamed("Z"))
    throw Rcpp::exception("Internal error in C_interpolate_delaunay: columns are not named XYZ.", false); // # nocov

  if (!L.containsElementNamed("X") || !L.containsElementNamed("Y"))
    throw Rcpp::exception("Internal error in C_interpolate_delaunay: columns are not named XY.", false); // # nocov

  if (scales.size() != 2 || offsets.size() != 2)
    throw Rcpp::exception("Internal error in C_delaunay: scales and/or offset are not of size 2.", false);

  if (scales[0] != scales[1])
    throw Rcpp::exception("Internal error in C_interpolate_delaunay: cannot triangulate points with different xy scale factors.", false); // # nocov

  // The point cloud
  NumericVector X = P["X"];
  NumericVector Y = P["Y"];
  NumericVector Z = P["Z"];

  // The location where to interpolate
  NumericVector x = L["X"];
  NumericVector y = L["Y"];

  if (X.size() < 3)
    throw Rcpp::exception("Internal error in C_interpolate_delaunay: cannot triangulate less than 3 points.", false); // # nocov

  // Use comparable distance instead of true distance (save the cost of sqrt)
  trim = trim * trim;

  // Need scale and offset to convert to integral numbers
  double scale_x = scales[0];
  double scale_y = scales[1];
  //double scale_z = scales[2];
  double offset_x = offsets[0];
  double offset_y = offsets[1];

  // Convert to boost point concept.
  // Coordinates are unscaled unoffseted for integer-based computation
  double dx,dy;
  int ix,iy;
  std::vector<point_int> points;
  for (int i = 0 ; i < X.size() ; i++)
  {
    dx = (X[i]-offset_x)/scale_x;
    dy = (Y[i]-offset_y)/scale_y;
    ix = std::round(dx);
    iy = std::round(dy);

    if (std::abs(dx - ix) > 1e-5 || std::abs(dy - iy) > 1e-5) {
      //Rprintf("%.10f, %d, %.10f \n", dx, ix, dx-ix);
      //Rprintf("%.10f, %d, %.10f \n", dy, iy, dy-iy);
      throw Rcpp::exception("Internal error in C_interpolate_delaunay: xy coordinates were not converted to integer. Scale factors are likely to be invalid.", false);
    }

    point_int p(ix, iy, i);
    points.push_back(p);
  }

  // Construction of the Voronoi Diagram (this step is fast).
  boost::polygon::voronoi_diagram<double> vd;
  construct_voronoi(points.begin(), points.end(), &vd);

  // Build a Spatial to retrieve point in triangles.
  GridPartition tree(x,y);

  // Progressbar and user interruption.
  bool abort = false;
  Progress pb(vd.num_vertices(), "Delaunay rasterization");

  // The output
  NumericVector z_out(x.size(), NA_REAL);

  // Loop over vertice of the tessellation, build the delaunay triangle and interpolate
  #pragma omp parallel for num_threads(ncpu)
  for (unsigned int i = 0 ; i < vd.num_vertices() ; ++i)
  {
    // Progress an check interrupt
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();

    std::vector<point_int> triangle;
    const voronoi_diagram<double>::const_vertex_iterator it = vd.vertices().begin() + i;
    const voronoi_diagram<double>::vertex_type& vertex = *it;
    const voronoi_diagram<double>::edge_type* edge = vertex.incident_edge();

    do
    {
      const voronoi_diagram<double>::cell_type* cell = edge->cell();
      assert(cell->contains_point());

      triangle.push_back(points[cell->source_index()]);

      if (triangle.size() == 3)
      {
        // Current triangle is ABC. Scale and offset back for double precision computation
        point_int &A_ = triangle[0];
        Point A(A_.x*scale_x+offset_x, A_.y*scale_y+offset_y, A_.id);
        point_int &B_ = triangle[1];
        Point B(B_.x*scale_y+offset_x, B_.y*scale_y+offset_y, B_.id);
        point_int &C_ = triangle[2];
        Point C(C_.x*scale_x+offset_x, C_.y*scale_y+offset_y, C_.id);

        // ABC is represented by vector u,v and w
        PointXYZ u(A.x - B.x, A.y - B.y, Z[A.id] - Z[B.id], 0);
        PointXYZ v(A.x - C.x, A.y - C.y, Z[A.id] - Z[C.id], 0);
        PointXYZ w(B.x - C.x, B.y - C.y, Z[B.id] - Z[C.id], 0);

        // Compute the AB, AC, BC edge comparable length
        double edge_AB = u.x * u.x + u.y * u.y;
        double edge_AC = v.x * v.x + v.y * v.y;
        double edge_BC = w.x * w.x + w.y * w.y;
        double edge_max = MAX(edge_AB, edge_AC, edge_BC);

        // Interpolate in this triangle if the longest edge fulfil requirements
        if (trim == 0 || edge_max < trim)
        {
          Triangle tri(A,B,C);

          // Find the points in this triangle
          std::vector<PointXYZ> pts;
          tree.lookup(tri,pts);

          if (pts.size() > 0)
          {
            // For each point, linear interpolation
            for (unsigned int j = 0 ; j < pts.size() ; j++)
            {
              PointXYZ& p = pts[j];

              PointXYZ n;
              n.x = u.y*v.z-u.z*v.y;
              n.y = u.z*v.x-u.x*v.z;
              n.z = u.x*v.y-u.y*v.x;

              PointXYZ nn;  // normalized normal vector
              double norm = std::sqrt(n.x*n.x + n.y*n.y + n.z*n.z);
              nn.x = n.x/norm;
              nn.y = n.y/norm;
              nn.z = n.z/norm;

              // If the z component is very very small it mean that the triangle is
              // Almost vertical. It is an artefact at the very edge of the dataset
              if (nn.z > min_normal_z)
              {
                double intercept = -(n.x*C.x + n.y*C.y + n.z*Z[C.id]);

                #pragma omp critical
                {
                  z_out[p.id] = -(p.x * n.x + p.y * n.y + intercept)/n.z;
                }
              }
            }
          }
        }

        triangle.erase(triangle.begin() + 1);
      }

      edge = edge->rot_next();


    } while (edge != vertex.incident_edge());
  }

  return z_out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix  C_tinfo(IntegerMatrix D, NumericMatrix P)
{
  if (P.nrow() < 3)
    throw Rcpp::exception("Internal error in 'C_tinfo()': wrong number of rows for P", false); // # no cov

  if (P.ncol() < 3)
    throw Rcpp::exception("Internal error in 'C_tinfo()': wrong number of columns for P", false); // # no cov

  if (D.ncol() < 3)
    throw Rcpp::exception("Internal error in 'C_tinfo()': wrong number of columns for D", false); // # no cov

  NumericMatrix N(D.nrow(), 7);
  std::fill(N.begin(), N.end(), NA_REAL);

  for (unsigned int i = 0, end = D.nrow() ; i < end ; i++)
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

// [[Rcpp::export(rng = false)]]
IntegerVector C_tsearch(IntegerMatrix D, NumericMatrix P, NumericMatrix X, int ncpu)
{
  if (P.nrow() < 3)
    throw Rcpp::exception("Internal error in 'C_tsearch()': wrong dimension for P", false);

  NumericVector x = X(_, 0);
  NumericVector y = X(_, 1);
  GridPartition tree(x, y);

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
    std::vector<PointXYZ> points;
    tree.lookup(triangle, points);

    // Return the id of the triangle
    #pragma omp critical
    {
      for(std::vector<PointXYZ>::iterator it = points.begin(); it != points.end(); it++)
      {
        int id = it->id;
        output(id) = k + 1;
      }
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return(output);
}
