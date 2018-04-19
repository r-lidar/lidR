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
		static QuadTree* create(const std::vector<double>, const std::vector<double>);
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

#endif //QT_H

