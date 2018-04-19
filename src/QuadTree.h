#ifndef QT_H
#define QT_H

#include <vector>
#include "Point.h"
#include "BoundingBox.h"

class QuadTree
{
	public:
		QuadTree(const double, const double, const double);
	  ~QuadTree();
		bool insert(const Point&);
		void rect_lookup(const double, const double, const double, const double, std::vector<Point*>&);
		void triangle_lookup(const Point&, const Point&, const Point&, std::vector<Point*>&);
		void circle_lookup(const double, const double, const double, std::vector<Point*>&);
		void knn_lookup(const double, const double, const int, std::vector<Point*>&);
		int count();
		BoundingBox bbox();

	private:
		int MAX_DEPTH;
	  double EPSILON;
	  double EPSILONSQ;
		int depth;
		int npoints;
		BoundingBox boundary;
		std::vector<Point> points;
		QuadTree* NE;
		QuadTree* NW;
		QuadTree* SE;
		QuadTree* SW;
		QuadTree* parent;
		QuadTree(const BoundingBox, const QuadTree*);
		void subdivide();
		void range_lookup(const BoundingBox, std::vector<Point*>&, const int);
		void getPointsSquare(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		void getPointsCircle(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		bool in_circle(const Point&, const Point&, const double);
		bool in_rect(const BoundingBox&, const Point&);
		bool in_triangle(const Point&, const Point&, const Point&, const Point&);
		double distanceSquarePointToSegment(const Point&, const Point&, const Point&);
};

template<typename T> static QuadTree* QuadTreeCreate(const T x, const T y);
template<typename T> static QuadTree* QuadTreeCreate(const T x, const T y)
{
  int n = x.size();

  double xmin = x[0];
  double ymin = y[0];
  double xmax = x[0];
  double ymax = y[0];

  for(int i = 0 ; i < n ; i++)
  {
    if(x[i] < xmin)
      xmin = x[i];
    else if(x[i] > xmax)
      xmax = x[i];
    if(y[i] < ymin)
      ymin = y[i];
    else if(y[i] > ymax)
      ymax = y[i];
  }

  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double range = xrange > yrange ? xrange/2 : yrange/2;

  QuadTree *tree = new QuadTree( (xmin+xmax)/2, (ymin+ymax)/2, range+0.01);

  for(int i = 0 ; i < n ; i++)
  {
    Point p(x[i], y[i], i);
    tree->insert(p);
  }

  return tree;
}

#endif //QT_H

