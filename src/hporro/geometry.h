#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <stdexcept>

namespace IncrementalDelaunay
{

struct Vec2
{
  double x, y, z;
  unsigned int fid;

  Vec2() : x(0), y(0), z(0), fid(0) {}
  Vec2(double x, double y) : x(x), y(y), z(0), fid(0) {}
  Vec2(double x, double y, double z) : x(x), y(y), z(z), fid(0) {}
  bool operator==(const Vec2& other) const { return x == other.x && y == other.y; }
  Vec2 operator-(const Vec2& other) const { return Vec2{x - other.x, y - other.y}; }
  Vec2 operator+(const Vec2& other) const { return Vec2{x + other.x, y + other.y}; }
  Vec2 operator/(double a) const { return Vec2{x / a, y / a}; }
  Vec2 operator*(double a) const { return Vec2{x * a, y * a}; }
  Vec2& operator+=(const Vec2& other) { x += other.x; y += other.y; return *this; }
  double dot(const Vec2& other) { return x*other.x + y*other.y; }
};

class Vertex
{
public:
  Vertex(const Vec2& v, int t) : pos(v), tri_index(t){}
  Vertex(const Vec2& v) : pos(v), tri_index(-1) {}
  Vertex() : tri_index(-1) {}
  Vec2 pos;
  int tri_index;
  void print();
};

class Triangle
{
public:
  Triangle(int v0,int v1,int v2,int t0,int t1,int t2) : frozen(false)
  {
    v[0] = v0;
    v[1] = v1;
    v[2] = v2;
    t[0] = t0;
    t[1] = t1;
    t[2] = t2;
  }
  Triangle() : frozen(false) {};
  int v[3]; // indices to the vertices vector
  int t[3]; // indices to the triangles vector
  bool frozen;
};

}

#endif
