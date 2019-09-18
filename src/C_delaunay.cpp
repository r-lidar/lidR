#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"
#include "Progress.h"
#include "GridPartition.h"

#include <boost/polygon/voronoi.hpp>

using boost::polygon::voronoi_diagram;
using namespace Rcpp;

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

typedef GridPartition SpatialIndex;

// [[Rcpp::export(rng = false)]]
IntegerMatrix C_delaunay(DataFrame P, NumericVector scales, NumericVector offsets)
{
  // The point cloud
  NumericVector X = P["X"];
  NumericVector Y = P["Y"];
  NumericVector Z = P["Z"];

  // Need scale and offset to convert to integral numbers
  double scale_x = scales[0];
  double scale_y = scales[1];
  double scale_z = scales[2];
  double offset_x = offsets[0];
  double offset_y = offsets[1];

  // Convert to boost point concept.
  // Coordinates are unscaled unoffseted for integer-based computation
  std::vector<point_int> points;
  for (int i = 0 ; i < X.size() ; i++)
  {
    points.push_back(point_int(std::round((X[i]-offset_x)/scale_x), std::round((Y[i]-offset_y)/scale_y), i));
  }

  // Construction of the Voronoi Diagram (this step is fast).
  boost::polygon::voronoi_diagram<double> vd;
  construct_voronoi(points.begin(), points.end(), &vd);

  IntegerMatrix output(vd.num_vertices(), 3);

  for (int i = 0 ; i < vd.num_vertices() ; ++i)
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
        output(i, 0) = triangle[0].id;
        output(i, 1) = triangle[1].id;
        output(i, 2) = triangle[2].id;

        triangle.erase(triangle.begin() + 1);
      }

      edge = edge->rot_next();

    } while (edge != vertex.incident_edge());
  }

  return output+1;
}

// [[Rcpp::export(rng = false)]]
NumericVector C_interpolate_delaunay(DataFrame P, DataFrame L, NumericVector scales, NumericVector offsets, double drop_z_below = 0, double drop_triangle_over = 0, int ncpu = 1)
{
  // The point cloud
  NumericVector X = P["X"];
  NumericVector Y = P["Y"];
  NumericVector Z = P["Z"];

  // The location where to interpolate
  NumericVector x = L["X"];
  NumericVector y = L["Y"];

  // The output
  NumericVector z_out(x.size(), NA_REAL);

  // Use comparable distance instead of true distance (save the cost of sqrt)
  drop_triangle_over = drop_triangle_over * drop_triangle_over;

  // Need scale and offset to convert to integral numbers
  double scale_x = scales[0];
  double scale_y = scales[1];
  double scale_z = scales[2];
  double offset_x = offsets[0];
  double offset_y = offsets[1];

  // Convert to boost point concept.
  // Coordinates are unscaled unoffseted for integer-based computation
  std::vector<point_int> points;
  for (int i = 0 ; i < X.size() ; i++)
  {
    if (drop_z_below == 0 || Z[i] >= drop_z_below)
      points.push_back(point_int(std::round((X[i]-offset_x)/scale_x), std::round((Y[i]-offset_y)/scale_y), i));
  }

  // Construction of the Voronoi Diagram (this step is fast).
  boost::polygon::voronoi_diagram<double> vd;
  construct_voronoi(points.begin(), points.end(), &vd);

  // Build a Spatial to retrieve point in triangles.
  SpatialIndex tree(x,y);

  // Progressbar and user interruption.
  bool abort = false;
  Progress pb(vd.num_vertices(), "Delaunay rasterization");

  // Loop over vertice of the tessellation, build the delaunay triangle and interpolate
  #pragma omp parallel for num_threads(ncpu)
  for (int i = 0 ; i < vd.num_vertices() ; ++i)
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
        double edge_max = max(edge_AB, edge_AC, edge_BC);

        // Interpolate in this triangle if the longest edge fullfil requirements
        if (drop_triangle_over == 0 || edge_max <= drop_triangle_over)
        {
          Triangle tri(A,B,C);

          // Find the points in this triangle
          std::vector<Point*> pts;
          tree.lookup(tri,pts);

          if (pts.size() > 0)
          {
            // For each point, linear interpolation
            for (int j = 0 ; j < pts.size() ; j++)
            {
              Point *p = pts[j];

              PointXYZ n;
              n.x = u.y*v.z-u.z*v.y;
              n.y = u.z*v.x-u.x*v.z;
              n.z = u.x*v.y-u.y*v.x;

              double intercept = -(n.x*C.x + n.y*C.y + n.z*Z[C.id]);

              #pragma omp critical
              {
                z_out[p->id] = -(p->x * n.x + p->y * n.y + intercept)/n.z;
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
