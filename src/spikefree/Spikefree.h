#ifndef SPIKEFREE_H
#define SPIKEFREE_H

#include <vector>
#include <functional>

#include "../hporro/constants.h"
#include "../hporro/delaunay.h"

namespace Spikefree
{

using Point = IncrementalDelaunay::Vec2;
using Grid = IncrementalDelaunay::Grid;
using Logger = std::function<void(const std::string&)>;

struct Parameters
{
  float d_f = 2;
  float h_b = 0.5;
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

struct Vec3
{
  double x, y, z;

  // Constructors
  Vec3() : x(0), y(0), z(0) {}
  Vec3(double x, double y) : x(x), y(y), z(0) {}
  Vec3(double x, double y, double z) : x(x), y(y), z(z) {}
  Vec3(const IncrementalDelaunay::Vec2& v) : x(v.x), y(v.y), z(v.z) {}
  Vec3 operator-(const Vec3& other) const { return Vec3(x - other.x, y - other.y, z - other.z); }
  Vec3 operator+(const Vec3& other) const { return Vec3(x + other.x, y + other.y, z + other.z); }
  Vec3 operator*(double scalar) const { return Vec3(x * scalar, y * scalar, z * scalar); }
  double dot(const Vec3& other) const { return x * other.x + y * other.y + z * other.z; }
  Vec3 cross(const Vec3& other) const {return Vec3(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x); }
  double length() const { return std::sqrt(x * x + y * y + z * z); }
  double distance(const Vec3& other) const { return (*this - other).length(); }
  Vec3 normalize() const { double len = length(); if (len == 0) return *this; return Vec3(x / len, y / len, z / len); }
};

class Spikefree
{
public:
  Spikefree(const Parameters& p, Bbox b);
  ~Spikefree();

  void set_logger(Logger l);
  bool pre_insert_point(double x, double y, double z);
  bool insert_point(double x, double y, double z);
  double get_z(double x, double y);

private:
  Parameters params;
  Logger logger;
  void log(const std::string& msg) const;

  Bbox bbox;
  double x_offset;
  double y_offset;
  double z_offset;
  double z_default;

  bool ready;
  double cur_h;

  std::vector<Point> high_points;

  IncrementalDelaunay::Grid spacing_grid;
  IncrementalDelaunay::Grid index_grid;
  IncrementalDelaunay::Triangulation* d;

  bool pre_tin();
  bool freeze(int t);
};

}

#endif // PTD_H
