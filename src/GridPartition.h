#ifndef SP_H
#define SP_H

#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"
#include "BoundingBox.h"

class GridPartition
{
  private:
    unsigned int npoints;
    unsigned int ncols;
    unsigned int nrows;
    bool use3D;
    double xmin;
    double xmax;
    double ymin;
    double ymax;
    double xres;
    double yres;
    double area;
    std::vector<std::vector<Point> > registry;
    Rcpp::NumericVector Z;

  public:
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector, const std::vector<bool>&);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector, const std::vector<bool>&);
    bool insert(const Point&);
    template<typename T> void lookup(T& shape, std::vector<Point*>&);
    template<typename T> void lookup(T& shape, std::vector<PointXYZ>&);
    void knn(const Point&, const unsigned int, std::vector<Point*>&);
    void knn(const Point&, const unsigned int, const double, std::vector<Point*>&);
    void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ>&);
    void knn(const PointXYZ&, const unsigned int, const double, std::vector<PointXYZ>&);

  private:
    int getCell(const double, const double);
};


template<typename T> void GridPartition::lookup(T& shape, std::vector<Point*>& res)
{
  double xmin = shape.bbox.center.x - shape.bbox.half_res.x;
  double xmax = shape.bbox.center.x + shape.bbox.half_res.x;
  double ymin = shape.bbox.center.y - shape.bbox.half_res.y;
  double ymax = shape.bbox.center.y + shape.bbox.half_res.y;

  int colmin  = std::floor((xmin - this->xmin) / xres);
  int colmax  = std::ceil((xmax - this->xmin) / xres);
  int rowmin  = std::floor((this->ymax - ymax) / yres);
  int rowmax  = std::ceil((this->ymax - ymin) / yres);
  int cell;

  res.clear();
  for (int col = std::max(colmin,0) ; col <= std::min(colmax, (int)ncols-1) ; col++) {
    for (int row = std::max(rowmin,0) ; row <= std::min(rowmax, (int)nrows-1) ; row++) {
      cell = row * ncols + col;
      for (std::vector<Point>::iterator it = registry[cell].begin() ; it != registry[cell].end() ; it++) {
        if (shape.contains(*it)) res.emplace_back(&(*it));
      }
    }
  }

  return;
}

template<typename T> void GridPartition::lookup(T& shape, std::vector<PointXYZ>& res)
{
  if (!use3D) throw(std::runtime_error("Internal error, trying to lookup in 3D in a 2D SpatialIndex"));

  double xmin = shape.bbox.center.x - shape.bbox.half_res.x;
  double xmax = shape.bbox.center.x + shape.bbox.half_res.x;
  double ymin = shape.bbox.center.y - shape.bbox.half_res.y;
  double ymax = shape.bbox.center.y + shape.bbox.half_res.y;

  int colmin  = std::floor((xmin - this->xmin) / xres);
  int colmax  = std::ceil((xmax - this->xmin) / xres);
  int rowmin  = std::floor((this->ymax - ymax) / yres);
  int rowmax  = std::ceil((this->ymax - ymin) / yres);
  int cell;

  res.clear();
  for (int col = std::max(colmin,0) ; col <= std::min(colmax, (int)ncols-1) ; col++) {
    for (int row = std::max(rowmin,0) ; row <= std::min(rowmax, (int)nrows-1) ; row++) {
      cell = row * ncols + col;
      for (std::vector<Point>::iterator it = registry[cell].begin() ; it != registry[cell].end() ; it++) {
        PointXYZ p(it->x, it->y, Z[it->id], it->id);
        if (shape.contains(p)) res.emplace_back(p);
      }
    }
  }

  return;
}

#endif //SP_H
