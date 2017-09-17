#ifndef QT_H
#define QT_H

#include <vector>

struct Point
{
	double x, y;
  int id;

	Point();
	Point(const double, const double);
	Point(const double, const double, const int);
};

struct BoundingBox
{
	Point center, half_res;

	BoundingBox();
	BoundingBox(const Point,const Point);
	bool contains(const Point&);
	bool intersects(const BoundingBox&);
};

double dist(const Point& lhs, const Point& rhs);

struct DistanceFunc
{
  DistanceFunc(const Point& _p) : p(_p) {}

  bool operator()(const Point* lhs, const Point* rhs) const
  {
    return dist(p, *lhs) < dist(p, *rhs);
  }

private:
  Point p;
};

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
