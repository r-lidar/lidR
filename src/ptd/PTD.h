#ifndef PTD_H
#define PTD_H

#include <vector>

#include "../hporro/constants.h"
#include "../hporro/delaunay.h"
#include "../nanoflann/nanoflann.h"

namespace PTD
{

using Point = IncrementalDelaunay::Vec2;
using Grid = IncrementalDelaunay::Grid;
using Logger = std::function<void(const std::string&)>;

struct Parameters
{
  float seed_resolution_search = 10.0;
  float max_iteration_angle = 30.0;
  float max_iteration_distance = 2.0;
  float spacing = 0.25;
  float buffer_size = 30.0;
  unsigned int max_iter = 50;
  unsigned int ncores = 1;
  bool verbose = false;
};

struct Bbox
{
  double xmin;
  double ymin;
  double zmin;
  double xmax;
  double ymax;
  double zmax;
};

class PTD
{
public:
  PTD(const Parameters& p, Bbox b);
  ~PTD();

  void set_logger(Logger l);
  bool insert_point(double x, double y, double z, unsigned int fid);
  bool run();
  std::vector<unsigned int> get_ground_fid() const;
  std::vector<unsigned int> get_spikes_fid() const;

private:
  void sort_by_z();
  void make_seeds();
  void make_buffer();
  void tin_buffer();
  void tin_seeds();
  void tin_densify();
  ///void rotate_scene();
  void detect_spikes(int k, double threshold);

  void sample_lowest_per_cell(const std::vector<Point>& points, IncrementalDelaunay::Grid& grid, std::vector<Point>& out);
  void deduplicate_seeds();

private:
  Parameters params;
  Logger logger;
  void log(const std::string& msg) const;

  Bbox bbox;
  double x_offset;
  double y_offset;
  double z_offset;
  double z_default;

  std::vector<Point> candidates;
  std::vector<Point> seeds;
  std::vector<Point> vbuff;
  std::vector<bool> inserted;
  std::vector<bool> is_spike;

  IncrementalDelaunay::Grid spacing_grid;
  IncrementalDelaunay::Grid index_grid;
  IncrementalDelaunay::Triangulation* d;
};

struct Vec3
{
  double x, y, z;

  // Constructors
  Vec3() : x(0), y(0), z(0) {}
  Vec3(double x, double y) : x(x), y(y), z(0) {}
  Vec3(double x, double y, double z) : x(x), y(y), z(z) {}
  Vec3(const Point& v) : x(v.x), y(v.y), z(v.z) {}
  Vec3 operator-(const Vec3& other) const { return Vec3(x - other.x, y - other.y, z - other.z); }
  Vec3 operator+(const Vec3& other) const { return Vec3(x + other.x, y + other.y, z + other.z); }
  Vec3 operator*(double scalar) const { return Vec3(x * scalar, y * scalar, z * scalar); }
  double dot(const Vec3& other) const { return x * other.x + y * other.y + z * other.z; }
  Vec3 cross(const Vec3& other) const {return Vec3(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x); }
  double length() const { return std::sqrt(x * x + y * y + z * z); }
  double distance(const Vec3& other) const { return (*this - other).length(); }
  Vec3 normalize() const
  {
    double len = length();
    if (len == 0) return *this;
    return Vec3(x / len, y / len, z / len);
  }
};

}

#endif // PTD_H
