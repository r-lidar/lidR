#ifndef QT_H
#define QT_H

#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"
#include "BoundingBox.h"

class QuadTree
{
	public:
	  QuadTree(Rcpp::NumericVector x, Rcpp::NumericVector y);
	  QuadTree(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector z);
	  QuadTree(Rcpp::S4 las);
	  ~QuadTree();
		bool insert(const Point&);
		template<typename T> void lookup(T& shape, std::vector<Point*>&); // solve issue with const correctness
		template<typename T> void lookup(T& shape, std::vector<PointXYZ>&); // solve issue with const correctness
		void knn(const Point&, const unsigned int, std::vector<Point*>&);
		void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ>&);
		int count();
		BoundingBox bbox();

	private:
	  bool use3D;
		int MAX_DEPTH;
		int depth;
		int npoints;
		BoundingBox boundary;
		std::vector<Point> points;
		QuadTree* NE;
		QuadTree* NW;
		QuadTree* SE;
		QuadTree* SW;
		QuadTree* parent;
		Rcpp::NumericVector Z;

private:
  QuadTree(const double, const double, const double);
  QuadTree(const BoundingBox, const QuadTree*);
  void subdivide();
  void init();
  void init(Rcpp::NumericVector x, Rcpp::NumericVector y);
  void init(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector z);
};

template<typename T> void QuadTree::lookup(T& shape, std::vector<Point*>& res)
{
  if(!boundary.intersects(shape.bbox))
    return;

  if(depth == MAX_DEPTH)
  {
    for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
    {
      if(shape.contains(*it))
        res.push_back(&(*it));
    }
    return;
  }

  if(NW == 0)
    return;

  NE->lookup(shape, res);
  NW->lookup(shape, res);
  SE->lookup(shape, res);
  SW->lookup(shape, res);

  return;
}


template<typename T> void QuadTree::lookup(T& shape, std::vector<PointXYZ>& res)
{
  if (!use3D)
    throw(std::runtime_error("Internal error, trying to lookup in 3D in a 2D QuadTree"));

  if(!boundary.intersects(shape.bbox))
    return;

  if(depth == MAX_DEPTH)
  {
    for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
    {
      PointXYZ p(it->x, it->y, Z[it->id], it->id);

      if(shape.contains(p))
        res.push_back(p);
    }
    return;
  }

  if(NW == 0)
    return;

  NE->lookup(shape, res);
  NW->lookup(shape, res);
  SE->lookup(shape, res);
  SW->lookup(shape, res);

  return;
}

#endif //QT_H

