#ifndef SP_H
#define SP_H

#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"

class GridPartition
{
  public:
    GridPartition(const Rcpp::S4 las);
    GridPartition(const Rcpp::S4 las, const std::vector<bool>& filter);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    template<typename T> void lookup(T& shape, std::vector<PointXYZ*>&);
    void knn(const Point&, const unsigned int, std::vector<PointXYZ*>&);
    void knn(const Point&, const unsigned int, const double, std::vector<PointXYZ*>&);
    void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ*>&);
    void knn(const PointXYZ&, const unsigned int, const double, std::vector<PointXYZ*>&);

  private:
    unsigned int npoints;
    unsigned int ncols;
    unsigned int nrows;
    unsigned int nlayers;
    double xmin;
    double xmax;
    double ymin;
    double ymax;
    double zmin;
    double zmax;
    double xres;
    double yres;
    double zres;
    double area;
    double volume;
    std::vector<double> filter;
    std::vector<std::vector<PointXYZ>> registry;

  private:
    int getCell(const PointXYZ&);
    bool insert(const PointXYZ&);
    void init(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    void setLayers(const int);
};


template<typename T> void GridPartition::lookup(T& shape, std::vector<PointXYZ*>& res)
{
  double xmin = shape.xmin;
  double xmax = shape.xmax;
  double ymin = shape.ymin;
  double ymax = shape.ymax;
  double zmin = shape.zmin;
  double zmax = shape.zmax;

  int colmin = std::floor((xmin - this->xmin) / xres);
  int colmax = std::ceil((xmax - this->xmin) / xres);
  int rowmin = std::floor((this->ymax - ymax) / yres);
  int rowmax = std::ceil((this->ymax - ymin) / yres);
  int laymin = std::floor((zmin - this->zmin) / zres);
  int laymax = std::ceil((zmax - this->zmin) / zres);
  int cell;

  res.clear();
  for (int col = std::max(colmin,0) ; col <= std::min(colmax, (int)ncols-1) ; col++) {
    for (int row = std::max(rowmin,0) ; row <= std::min(rowmax, (int)nrows-1) ; row++) {
      for (int lay = std::max(laymin,0) ; lay <= std::min(laymax, (int)nlayers-1) ; lay++) {
        cell = lay * nrows * ncols + row * ncols + col;
        for (std::vector<PointXYZ>::iterator it = registry[cell].begin() ; it != registry[cell].end() ; it++) {
          if (shape.contains(*it))
            res.emplace_back(&(*it));
        }
      }
    }
  }

  return;
}

#endif //SP_H
