#ifndef OT_H
#define OT_H

#include <Rcpp.h>
#include <limits>

#include "Point.h"
#include "Shapes.h"
#include "Bucket.h"
#include "Node.h"

namespace lidR
{

class Octree
{
  public:
    Octree();
    Octree(const Rcpp::S4 las);
    Octree(const Rcpp::S4 las, const std::vector<bool>& filter);
    Octree(const Rcpp::NumericVector, const Rcpp::NumericVector);
    Octree(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    template<typename T> void lookup(T& shape, std::vector<PointXYZ>&);
    void knn(const PointXY&, const unsigned int, std::vector<PointXYZ>&);
    void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ>&);
    void knn(const PointXY&, const unsigned int, const double, std::vector<PointXYZ>&);
    void knn(const PointXYZ&, const unsigned int, const double, std::vector<PointXYZ>&);

  private:
    std::vector<Node::Ocnode> heap;
    std::vector<bool> filter;
    unsigned char ROOT_LEVEL;
    unsigned char MAX_VAL;
    double xmin,ymin,xmax,ymax,zmin,zmax;

  private:
    Node::Ocnode* subdivide(Node::Ocnode* node);
    bool insert(Node::Ocnode* node, const PointXYZ& p);
    template<typename T> Node::Ocnode* locate_node(const T p);
    template<typename T> Node::Ocnode* locate_nearest_node(const T p);
    template<typename T> Node::Ocnode* locate_region(const T shape);
    Node::Ocnode* traverse(Node::Ocnode* cell, unsigned char nextLevel, unsigned char xLocCode, unsigned char yLocCode, unsigned char zLocCode);
    Node::Ocnode* traverse_to_level(Node::Ocnode* cell, unsigned char nextLevel, unsigned char x0LocCode, unsigned char y0LocCode, unsigned char z0LocCode, unsigned char level);
    void build(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    void knn(Bucket::KnnBucket&);
    void harvest_knn(Node::Ocnode* node, Bucket::KnnBucket& knn, unsigned char excludepos);
    template<typename T> void harvest_in(Node::Ocnode* node, T& shape, std::vector<PointXYZ>& res);
    template<typename T> bool contains(Node::Ocnode*, const T&);
    template<typename T> double distance(Node::Ocnode*, const T&);
    template<typename T> bool intersects(Node::Ocnode* node, T& shape);
    void compute_bbox(Node::Ocnode* node, double& xmn, double& xmx, double& ymn, double& ymx, double& zmn, double& zmx);
};

inline Octree::Octree()
{
}

inline Octree::Octree(const Rcpp::S4 las)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];

  //Initialize a filter with true (all points are kept)
  filter.resize(data.nrow());
  std::fill(filter.begin(), filter.end(), true);

  build(x,y,z);
}

inline Octree::Octree(const Rcpp::S4 las, const std::vector<bool>& f)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];

  //Initialize a filter to discard some points on the fly
  std::copy(f.begin(), f.end(), std::back_inserter(filter));

  build(x,y,z);
}

inline Octree::Octree(const Rcpp::NumericVector x, const Rcpp::NumericVector y)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes."); // # nocov

  // Initialize a filter with true (all points are kept)
  filter.resize(x.size());
  std::fill(filter.begin(), filter.end(), true);

  // Create a dummy z vector with 0s
  Rcpp::NumericVector z(x.size());

  build(x,y,z);
}

inline Octree::Octree(const Rcpp::NumericVector x, const Rcpp::NumericVector y, const Rcpp::NumericVector z)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes."); // # nocov

  if (x.size() != z.size())
    Rcpp::stop("Internal error in spatial index: x and z have different sizes."); // # nocov

  // Initialize a filter with true (all points are kept)
  filter.resize(x.size());
  std::fill(filter.begin(), filter.end(), true);

  build(x,y,z);
}

template<typename T> void Octree::lookup(T& shape, std::vector<PointXYZ>& res)
{
  Node::Ocnode* node = locate_region(shape);
  if (node != 0) harvest_in(node, shape, res);
  return;
}

inline void Octree::knn(const PointXY& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  knn(p, k, 0, res);
}

inline void Octree::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  knn(p, k, 0, res);
}

inline void Octree::knn(const PointXY& p, const unsigned int k, const double radius, std::vector<PointXYZ>& res)
{
  Bucket::KnnBucket bucket(p, k, radius);
  knn(bucket);

  res.clear();
  PointXYZ pp(p.x, p.y, 0, 0);
  for(unsigned int i = 0 ; i < bucket.k ; i++) res.push_back(*bucket.bucket[i]);
  std::sort(res.begin(), res.end(), DSort2D<PointXYZ>(pp));
  return;
}

inline void Octree::knn(const PointXYZ& p, const unsigned int k, const double radius, std::vector<PointXYZ>& res)
{
  Bucket::KnnBucket bucket(p, k, radius);
  knn(bucket);

  res.clear();
  for(unsigned int i = 0 ; i < bucket.k ; i++) res.push_back(*bucket.bucket[i]);
  std::sort(res.begin(), res.end(), DSort3D<PointXYZ>(p));
  return;
}

inline void Octree::knn(Bucket::KnnBucket& bucket)
{
  Node::Ocnode* node = locate_nearest_node(bucket.pref);

  if (node == 0) Rcpp::stop("Internal error: no node found"); // # nocov

  if (node->level == 0)
    for (auto &pt : node->points)
      bucket.push(pt);

  while (node->level < ROOT_LEVEL)
  {
    unsigned int pos = node->pos;
    node = &heap[node->parent];

    harvest_knn(node, bucket, pos);
   }

  return;
}

inline void Octree::build(Rcpp::NumericVector x, Rcpp::NumericVector y,  Rcpp::NumericVector z)
{
  if (x.size() != y.size())
    throw(std::runtime_error("Internal error in Octree. x and y have different sizes.")); // # no cov

  if (x.size() != z.size())
    Rcpp::stop("Internal error in spatial index: x and z have different sizes."); // # nocov

  // Compute the bounding box of the tree
  xmin = Rcpp::min(x);
  ymin = Rcpp::min(y);
  zmin = Rcpp::min(z);
  xmax = Rcpp::max(x);
  ymax = Rcpp::max(y);
  zmax = Rcpp::max(z);

  // Check if we do no have a 0 width range for x or y or z otherwise we will make divisions by 0
  // This may happen in rare cases with 1 point or with x|y|z aligned points
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double zrange = zmax - zmin;

  if (xrange == 0)
  {
    xmin = xmin - 1;
    xmax = xmax + 1;
  }

  if (yrange == 0)
  {
    ymin = ymin - 1;
    ymax = ymax + 1;
  }

  if (zrange == 0)
  {
    zmin = zmin - 1;
    zmax = zmax + 1;
  }

  // Check the xyz ratio. If xyz ratio != 1 the Octree is cuboid and each box
  // is cuboid as well. This is not a problem but if one direction is very narrow
  // this will lead to an over-subdivision on one axis. It is likely to be suboptimal so
  // we enforce a maximum ratio of 2
  if (xrange > 2*yrange)
  {
    ymax = ymax + (xrange/2-yrange);
    yrange = ymax - ymin;
  }
  else if (yrange > 2*xrange)
  {
    xmax = xmax + (yrange/2-xrange);
    xrange = xmax - xmin;
  }

  if (std::min(xrange, yrange) > 2*zrange)
  {
    zmax = zmax + (std::min(xrange, yrange) - zrange);
    zrange = zmax - zmin;
  }

  // Estimate the depth of the Octree
  // Not more than 8 because we are using unsigned char to locate the nodes.
  // In practice 6 level is enough
  // 6 levels -> 8^6 = 262144 leaves -> 1+8+64+512+4096+32768+262144=299592 quadrants
  unsigned int n = x.size();
  unsigned int num_levels = (n > 0) ? std::floor(std::log(n)/std::log(8)) : 0;
  num_levels = (num_levels >= 1) ? num_levels : 1;
  num_levels = (num_levels >= 6) ? 6 : num_levels;

  ROOT_LEVEL = num_levels - 1;
  MAX_VAL = 1 << ROOT_LEVEL;

  if (x.size() == 0)
  {
    xmin = 0;
    xmax = 0;
    ymin = 0;
    ymax = 0;
    zmin = 0;
    zmax = 0;
    num_levels = 1;
    ROOT_LEVEL = 0;
    MAX_VAL = 1;
  }

  // Compute the maximum number of node given the depth and alloc memory for
  // 1/4 of the maximum number of nodes
  unsigned int node_count = 0;
  for (unsigned int i = 0 ; i <= num_levels ; ++i) { node_count += std::pow(8, i); }
  heap.reserve(std::ceil(node_count/4));

  Node::Ocnode root;
  root.level = ROOT_LEVEL;
  heap.push_back(root);

  //Rprintf("Octree [%.2lf, %.2lf] x [%.2lf, %.2lf] x [%.2lf, %.2lf] with %d level and %d node \n",  xmin,xmax,ymin,ymax,zmin,zmax,ROOT_LEVEL+1, node_count);

  // Built the Octree
  Node::Ocnode* node;
  for(int i = 0 ; i < x.size() ; i++)
  {
    if (filter[i])
    {
      PointXYZ p(x[i], y[i], z[i], i);
      node = locate_node(p);
      if (node->level > 0)
      {
        if (!insert(&heap[0], p))
          Rcpp::stop("Internal error in Octree. Point not inserted."); // # no cov
      }
      else
      {
        node->points.push_back(p);
      }
    }
  }

  return;
}

inline bool Octree::insert(Node::Ocnode* node, const PointXYZ& p)
{
  if (node->level == 0)
  {
    node->points.push_back(p);
    return true;
  }

  if (node->firstChild == -1 && node->level > 0)
    node = subdivide(node);

  for (unsigned int i = 0; i < 8; ++i)
  {
    if (contains(&heap[node->firstChild+i], p))
    {
      if (insert(&heap[node->firstChild+i], p))
        return true;
    }
  }

  return false; // # no cov
}

inline Node::Ocnode* Octree::subdivide(Node::Ocnode* node)
{
  node->firstChild = std::distance(heap.begin(), heap.end());

  int parent_pos = std::distance(&heap[0], node);
  unsigned char parent_level = node->level;
  unsigned char parent_xloc = node->xLocCode;
  unsigned char parent_yloc = node->yLocCode;
  unsigned char parent_zloc = node->zLocCode;

  for (unsigned char child_pos = 0 ; child_pos < 8 ; ++child_pos)
  {
    Node::Ocnode n(parent_level, parent_xloc, parent_yloc, parent_zloc, parent_pos, child_pos);
    heap.emplace_back(n);
  }

  return &heap[parent_pos];
}

template<typename T>  Node::Ocnode* Octree::locate_node(const T p)
{
  double x = (p.x - xmin)/(xmax-xmin);
  double y = (p.y - ymin)/(ymax-ymin);
  double z = (p.z - zmin)/(zmax-zmin);

  if (x < 0) return 0;
  if (x > 1) return 0;
  if (y < 0) return 0;
  if (y > 1) return 0;
  if (z < 0) return 0;
  if (z > 1) return 0;

  x = std::max(x, 0.0);
  y = std::max(y, 0.0);
  z = std::max(z, 0.0);

  //----Determine the x and y locational codes of the point's position. Refer
  //----to [King2001] for more efficient methods for converting floating point
  //----numbers to integers.
  unsigned char xLocCode = (x == 1) ? MAX_VAL-1 : (unsigned char) (x * MAX_VAL);
  unsigned char yLocCode = (y == 1) ? MAX_VAL-1 : (unsigned char) (y * MAX_VAL);
  unsigned char zLocCode = (z == 1) ? MAX_VAL-1 : (unsigned char) (z * MAX_VAL);

  //----Follow the branching patterns of the locational codes from the root cell
  //----to locate the leaf cell containing p
  unsigned int nextLevel = ROOT_LEVEL - 1;

  return traverse(&heap[0], nextLevel, xLocCode,yLocCode, zLocCode);
}

template<typename T>  Node::Ocnode* Octree::locate_nearest_node(const T p)
{
  if (contains(&heap[0], p)) return locate_node(p);

  PointXYZ q(p.x,p.y,p.z);
  if (p.x < xmin) q.x = xmin;
  else if (p.x > xmax) q.x = xmax;

  if (p.y < ymin) q.y = ymin;
  else if (p.y > ymax) q.y = ymax;

  if (p.z < zmin) q.z = zmin;
  else if (p.z > zmax) q.z = zmax;

  return locate_node(q);
}

template<typename T>  Node::Ocnode* Octree::locate_region(T shape)
{
  // Scale coordinates to simulate a [0,1] x [0,1] x [0, 1] Octree
  double bbxmin = (shape.xmin - xmin)/(xmax-xmin);
  double bbxmax = (shape.xmax - xmin)/(xmax-xmin);
  double bbymin = (shape.ymin - ymin)/(ymax-ymin);
  double bbymax = (shape.ymax - ymin)/(ymax-ymin);
  double bbzmin = (shape.zmin - zmin)/(zmax-zmin);
  double bbzmax = (shape.zmax - zmin)/(zmax-zmin);

  if (bbxmax < 0 || bbxmin > 1 || bbymax < 0 || bbymin > 1 || bbzmax < 0 || bbzmin > 1)
    return 0;

  bbxmin = std::max(bbxmin, 0.0);
  bbxmax = std::min(bbxmax, 1.0);
  bbymin = std::max(bbymin, 0.0);
  bbymax = std::min(bbymax, 1.0);
  bbzmin = std::max(bbzmin, 0.0);
  bbzmax = std::min(bbzmax, 1.0);

  //----Determine the x and y locational codes of the region boundaries
  unsigned char x0LocCode = (bbxmin == 1) ? MAX_VAL-1 : (unsigned char) (bbxmin * MAX_VAL);
  unsigned char y0LocCode = (bbymin == 1) ? MAX_VAL-1 : (unsigned char) (bbymin * MAX_VAL);
  unsigned char z0LocCode = (bbzmin == 1) ? MAX_VAL-1 : (unsigned char) (bbzmin * MAX_VAL);
  unsigned char x1LocCode = (bbxmax == 1) ? MAX_VAL-1 : (unsigned char) (bbxmax * MAX_VAL);
  unsigned char y1LocCode = (bbymax == 1) ? MAX_VAL-1 : (unsigned char) (bbymax * MAX_VAL);
  unsigned char z1LocCode = (bbzmax == 1) ? MAX_VAL-1 : (unsigned char) (bbzmax * MAX_VAL);

  //----Determine the XOR'ed pairs of locational codes of the region boundaries
  unsigned char xDiff = x0LocCode ^ x1LocCode;
  unsigned char yDiff = y0LocCode ^ y1LocCode;
  unsigned char zDiff = z0LocCode ^ z1LocCode;

  //----Determine the level of the smallest possible cell entirely containing
  //----the region
  unsigned char  level = ROOT_LEVEL;
  unsigned char xlevel = ROOT_LEVEL;
  unsigned char ylevel = ROOT_LEVEL;
  unsigned char zlevel = ROOT_LEVEL;
  unsigned char minLevel = ROOT_LEVEL;
  if (xDiff == 0 && yDiff == 0 && zDiff == 0)
  {
    minLevel = 0;
  }
  else
  {
    while (!(xDiff & (1 << xlevel)) && xlevel) xlevel--;
    while (!(yDiff & (1 << ylevel)) && ylevel) ylevel--;
    while (!(zDiff & (1 << zlevel)) && zlevel) zlevel--;
    minLevel = std::max({xlevel, ylevel, zlevel});
  }

  //----Follow the branching patterns of the locational codes of v0 from the
  //----root cell to the smallest cell entirely containing the region
  level = minLevel;
  unsigned char nextLevel = ROOT_LEVEL - 1;
  return traverse_to_level(&heap[0], nextLevel, x0LocCode, y0LocCode, z0LocCode, level);
}

inline Node::Ocnode* Octree::traverse(Node::Ocnode* node, unsigned char nextLevel, unsigned char xLocCode, unsigned char yLocCode, unsigned char zLocCode)
{
  while (node->firstChild != -1)
  {
    unsigned char lr = (xLocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char tb = (yLocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char hl = (zLocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char childIndex = lr + (tb << 1) + (hl << 2);
    nextLevel--;
    node = &heap[node->firstChild + childIndex];
  }

  return node;
}

inline Node::Ocnode* Octree::traverse_to_level(Node::Ocnode* node, unsigned char nextLevel, unsigned char x0LocCode, unsigned char y0LocCode, unsigned char z0LocCode, unsigned char level)
{
  unsigned char n = (nextLevel) - (level) + 1;

  while (n--)
  {
    unsigned char lr = (x0LocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char tb = (y0LocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char hl = (z0LocCode &  (1 << nextLevel)) >> nextLevel;
    unsigned char childIndex = lr + (tb << 1) + (hl << 2);
    nextLevel--;
    node = &heap[node->firstChild + childIndex];
    if (node->firstChild == -1) break;
  }

  return node;
}

inline void Octree::harvest_knn(Node::Ocnode* node, Bucket::KnnBucket& bucket, unsigned char excludepos)
{

  // If it is a leaf we harvest all the point and push thin the knn bucket
  if (node->firstChild == -1)
  {
    for (auto &pt : node->points)
      bucket.push(pt);

    return;
  }

  // Else we recurse the heap

  // Sort the 8 quadrants by distance to the reference point to statistically
  // reduce the number of useless comparisons
  double d[8];
  std::vector<unsigned char> idx = {0,1,2,3,4,5,6,7};
  for(auto i : idx) d[i] = distance(&heap[node->firstChild + i], bucket.pref);
  std::stable_sort(idx.begin(), idx.end(), [&d](size_t i1, size_t i2) {return d[i1] < d[i2];});

  for(auto i : idx)
  {
    if (excludepos == i)
      continue;

    if (bucket.max_dist >= d[i])
      harvest_knn(&heap[node->firstChild + i], bucket, 9);
    else
      break;
  }

  return;
}

template<typename T> void Octree::harvest_in(Node::Ocnode* node, T& shape, std::vector<PointXYZ>& res)
{
  if (node->firstChild == -1)
  {
    for (std::vector<PointXYZ>::iterator it = node->points.begin() ; it != node->points.end() ; it++)
    {
      if (shape.contains(*it))
        res.emplace_back(*it);
    }
  }
  else
  {
    for (unsigned int i = 0 ; i < 8 ; ++i)
    {
      if (intersects(&heap[node->firstChild + i], shape))
        harvest_in(&heap[node->firstChild + i], shape, res);
    }
  }

  return;
}

template<typename T> bool Octree::contains(Node::Ocnode* node, const T& p)
{
  double xmin, xmax, ymin, ymax, zmin, zmax;
  compute_bbox(node, xmin, xmax, ymin, ymax, zmin, zmax);

  return(p.x >= xmin - EPSILON &&
         p.x <= xmax + EPSILON &&
         p.y >= ymin - EPSILON &&
         p.y <= ymax + EPSILON &&
         p.z >= zmin - EPSILON &&
         p.z <= zmax + EPSILON);
}

template<typename T> double Octree::distance(Node::Ocnode* node, const T& p)
{
  double xmin, xmax, ymin, ymax, zmin, zmax;
  compute_bbox(node, xmin, xmax, ymin, ymax, zmin, zmax);

  double dx;
  if (p.x < xmin)
    dx = xmin - p.x;
  else if (p.x > xmax)
    dx = p.x - xmax;
  else
    dx = 0;

  double dy;
  if (p.y < ymin)
    dy = ymin - p.y;
  else if (p.y > ymax)
    dy = p.y - ymax;
  else
    dy = 0;

  double dz;
  if (p.z < zmin)
    dz = zmin - p.z;
  else if (p.z > zmax)
    dz = p.z - zmax;
  else
    dz = 0;

  return std::sqrt(dx*dx + dy*dy +dz*dz);
}

inline void Octree::compute_bbox(Node::Ocnode* node, double& xmn, double& xmx, double& ymn, double&ymx, double& zmn, double& zmx)
{
  double W = (xmax-xmin);
  double H = (ymax-ymin);
  double D = (zmax-zmin);
  double width = W/(1 << (ROOT_LEVEL - node->level));
  double height = H/(1 << (ROOT_LEVEL - node->level));
  double depth = D/(1 << (ROOT_LEVEL - node->level));
  xmn = (double)node->xLocCode/MAX_VAL * W  + xmin;
  ymn = (double)node->yLocCode/MAX_VAL * H  + ymin ;
  zmn = (double)node->zLocCode/MAX_VAL * D  + zmin ;
  xmx = xmn + width;
  ymx = ymn + height;
  zmx = zmn + depth;
  return;
}

template<typename T> bool Octree::intersects(Node::Ocnode* node, T& shape)
{
  double xmin, xmax, ymin, ymax, zmin, zmax;
  compute_bbox(node, xmin, xmax, ymin, ymax, zmin, zmax);
  return !(shape.xmin > xmax || shape.xmax < xmin || shape.ymin > ymax || shape.ymax < ymin || shape.zmin > zmax || shape.zmax < zmin);
}

}

#endif //OT_H

