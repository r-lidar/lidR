#include <stdexcept>
#include <limits>
#include <algorithm>
#include <numeric>
#include <random>
#include <cmath>
#include <chrono>

#include "Spikefree.h"

namespace Spikefree
{

using clock = std::chrono::steady_clock;


Spikefree::Spikefree(const Parameters& p, Bbox b)
{
  if (p.d_f < 0.0) throw std::invalid_argument("Freeze size must be >= 0");
  if (p.h_b <= 0.0) throw std::invalid_argument("Z buffer size must be >= 0");

  // Offset for floating point accuracy
  x_offset = std::round((b.xmin+b.xmax)/2);
  y_offset = std::round((b.ymin+b.ymax)/2);
  z_offset = std::round((b.zmin+b.zmax)/2);
  bbox = b;
  bbox.xmin -= x_offset;
  bbox.xmax -= x_offset;
  bbox.ymin -= y_offset;
  bbox.ymax -= y_offset;

  // Spike free parameters
  params = p;
  params.d_f = params.d_f *  params.d_f; //squared comparable distance

  // Spatial index for the triangulation
  index_grid = IncrementalDelaunay::Grid(bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax, 1);

  // Triangulation
  d = new IncrementalDelaunay::Triangulation(index_grid);

  logger = nullptr;

  // Grid to retain highest point per grid cell during pre-densification
  float res = 10.0f;
  if (params.d_f > 0) res = std::min(20*std::sqrt(params.d_f), 10.0f);
  spacing_grid = Grid(bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax, res);
  high_points.resize(spacing_grid.get_ncells());
  for(auto& p : high_points) { p.z = -std::numeric_limits<double>::infinity(); }

  // Grid for locally adaptive spikefree
  if (params.d_f == 0)
  {
    point_density_grid = Grid(bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax, 5);
    pulse_spacing.resize(point_density_grid.get_ncells());
    std::fill(pulse_spacing.begin(), pulse_spacing.end(), 0);
  }

  // state
  cur_h = std::numeric_limits<double>::infinity();
  ready = false;
}

Spikefree::~Spikefree() { if (d) delete d; }

void Spikefree::set_logger(Logger l)
{
  logger = l;
}

bool Spikefree::pre_insert_point(double x, double y, double z)
{
  int cell = spacing_grid.cell_from_xy(x-x_offset, y-y_offset);

  if (cell < 0 || cell >= (int)high_points.size()) return false;

  if (z > high_points[cell].z)
  {
    auto& pt = high_points[cell];
    pt.x = x - x_offset;
    pt.y = y - y_offset;
    pt.z = z;
  }


  if (params.d_f == 0)
  {
    cell = point_density_grid.cell_from_xy(x-x_offset, y-y_offset);
    pulse_spacing[cell] += 0.04f; // 1/(5x5)
  }

  return true;
}

bool Spikefree::pre_tin()
{
  auto new_end = std::remove_if(high_points.begin(), high_points.end(), [](const IncrementalDelaunay::Vec2& p) {
    return p.z == -std::numeric_limits<double>::infinity();
  });
  high_points.erase(new_end,  high_points.end());

  d->desactivate_spatial_index();
  for (auto& p : high_points)
  {
    int t = d->findContainerTriangleFast(p);
    if (!d->delaunayInsertion(p, t))
      throw std::runtime_error("Internal error: point not inserted in pre_tin()");
  }
  d->activate_spatial_index();
  ready = true;

  if (params.d_f == 0)
  {
    for (auto& v : pulse_spacing)
    {
      if (v < 0.05)
        v = 10;
      else
        v = 1/std::sqrt(v);
    }
  }

  return true;
}

bool Spikefree::insert_point(double x, double y , double z)
{
  if (!ready) pre_tin();
  if (z > cur_h) throw std::runtime_error("Point are not inserted in Z order");

  IncrementalDelaunay::Vec2 p;
  p.x = x - x_offset;
  p.y = y - y_offset;
  p.z = z;

  cur_h = p.z;

  int t = d->findContainerTriangleFast(p);
  if (t < 0) return false;
  if (d->is_frozen(t)) return false;

  if (freeze(t, p.x, p.y))
  {
    d->set_frozen(t, true);
    return false;
  }

  d->delaunayInsertion(p, t);

  return true;
}

void Spikefree::log(const std::string& msg) const
{
  if (params.verbose && logger)
    logger(msg);
}

double Spikefree::get_z(double x, double y)
{
  IncrementalDelaunay::Vec2 p(x-x_offset, y-y_offset);

  int t = d->findContainerTriangleFast(p);

  if (t != -1)
  {
    IncrementalDelaunay::Triangle& tri = d->triangles[t];

    if (tri.v[0] <= 3 || tri.v[1] <= 3 || tri.v[2] <= 3) return std::numeric_limits<double>::quiet_NaN();

    Vec3 A(d->vertices[tri.v[0]].pos);
    Vec3 B(d->vertices[tri.v[1]].pos);
    Vec3 C(d->vertices[tri.v[2]].pos);

    // Linear Interpolation using Barycentric Coordinates ---
    double det = (B.x - A.x) * (C.y - A.y) - (C.x - A.x) * (B.y - A.y);
    if (std::abs(det) < 1e-9) { return std::max({A.z, B.z, C.z}); }
    double w_A = ((B.y - C.y) * (p.x - C.x) + (C.x - B.x) * (p.y - C.y)) / det;
    double w_B = ((C.y - A.y) * (p.x - C.x) + (A.x - C.x) * (p.y - C.y)) / det;
    double w_C = 1.0 - w_A - w_B;
    return (w_A * A.z) + (w_B * B.z) + (w_C * C.z);
  }

  return std::numeric_limits<double>::quiet_NaN();
}

bool Spikefree::freeze(int t, double x, double y)
{
  const IncrementalDelaunay::Triangle& tri = d->triangles[t];

  const Vec3& posA = d->vertices[tri.v[0]].pos;
  const Vec3& posB = d->vertices[tri.v[1]].pos;
  const Vec3& posC = d->vertices[tri.v[2]].pos;

  IncrementalDelaunay::Vec2 a2d = {posA.x, posA.y};
  IncrementalDelaunay::Vec2 b2d = {posB.x, posB.y};
  IncrementalDelaunay::Vec2 c2d = {posC.x, posC.y};

  IncrementalDelaunay::Vec2 u = a2d - b2d;
  IncrementalDelaunay::Vec2 v = a2d - c2d;
  IncrementalDelaunay::Vec2 w = b2d - c2d;

  double edge_AB = u.dot(u);
  double edge_AC = v.dot(v);
  double edge_BC = w.dot(w);

  double sq_max_edge = std::max({edge_AB, edge_AC, edge_BC});
  double min_h = std::min({posA.z, posB.z, posC.z});

  // Evaluate freeze condition
  double d_f = params.d_f;
  if (d_f == 0)
  {
    int cell = point_density_grid.cell_from_xy(x, y);
    float ps = pulse_spacing[cell];
    d_f = params.la_slope*ps+params.la_intercept ;
    d_f = d_f*d_f;
  }

  return (sq_max_edge <= d_f) && ((min_h - cur_h) >= params.h_b);
}

}
