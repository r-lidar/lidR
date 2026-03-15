#ifndef DELAUNAY_H
#define DELAUNAY_H

#include <vector>

#include "geometry.h"
#include "index.h"

namespace IncrementalDelaunay
{

class Triangulation
{
public:
  Triangulation(const Grid& index);
  ~Triangulation();
  bool delaunayInsertion(const Vec2& point, int tri_index = -1);
  int findContainerTriangle(const Vec2& p, int prop) const;
  int findContainerTriangleFast(const Vec2& p) const;
  void desactivate_spatial_index();
  void activate_spatial_index();
  void reset_dirty_cells();
  void set_frozen(int t, bool frozen);
  bool is_frozen(int t) const;
  const std::vector<bool>& get_dirty_cells() const;
  //void write(const std::string& filename) const;

  Vertex *vertices;
  Triangle *triangles;
  int vcount = 0;
  int tcount = 0;

private:
  void addPointInside(const Vec2& point,int);
  void addPointInEdge(const Vec2& point, int t1, int t2);
  void addPointInEdge(const Vec2& point, int t);

  void legalize(const std::vector<int>& input_triangles);
  bool flip(int t1, int t2);

  bool isInside(int t, Vec2) const; //checks if a Vec2 is inside the triangle in the index t
  bool isInside(int t, int v) const;
  bool isInEdge(int t, Vec2) const; //checks if a Vec2 is in a edge of a triangle

  bool isConvexBicell(int t1, int t2); // Checks if a bicell is convex
  bool isCCW(int f) const; // check if a triangle, in the position f of the triangles array, is ccw
  bool areConnected(int,int) const;

  //double orient2d(const Vec2& pa, const Vec2& pb, const Vec2& pc) const;
  double inCircle(const Vec2& a, const Vec2& b, const Vec2& c, const Vec2& d) const;
  bool pointInSegment(const Vec2& p, const Vec2& p1, const Vec2& p2) const;

  void remem(); // checks if more memory is needed, and if it is needed, allocates more memory.

  int maxTriangles;
  int maxVertices;
  int incount = 0;

  float a;
  Vec2 p0,p1,p2,p3;

  // Spatial indexing
  const Grid& index;
  bool index_active;
  std::vector<std::vector<int>> grid;
  std::vector<bool> dirty_cells;
  void indexTriangle(int t);
  void unindexTriangle(int t);

  std::vector<std::pair<int, int>> m_flip_stack;
  std::vector<int> m_temp_cells;
};

}

#endif
