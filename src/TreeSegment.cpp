#include "TreeSegment.h"

namespace ptrees
{

TreeSegment::TreeSegment()
{
  nbPoints = 0;
  area = 0;
  k = 0;
  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::TreeSegment(int k)
{
  nbPoints = 0;
  area = 0;
  this->k = k;

  PointXYZ pmax(DOUBLE_XMIN,DOUBLE_XMIN,DOUBLE_XMIN, 0);
  Zmax = pmax;
  PointXYZ pmin(DOUBLE_XMAX,DOUBLE_XMAX,DOUBLE_XMAX, 0);
  Zmin = pmin;

  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::TreeSegment(PointXYZ &pt, int k)
{
  nbPoints = 1;
  area = 0;
  this->k = k;

  point_t apex(pt.x, pt.y);
  boost::geometry::append(convex_hull, apex);

  Zmax = pt;
  Zmin = pt;

  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::~TreeSegment() {}

void TreeSegment::compute_area()
{
  if (nbPoints <=  2)
    return;

  area = boost::geometry::area(convex_hull);
}

double TreeSegment::compute_area_increment(PointXYZ &pt)
{
  point_t p(pt.x, pt.y);

  if(boost::geometry::covered_by(p, convex_hull))
    return 0;

  polygon old_hull(convex_hull);
  polygon new_hull;
  boost::geometry::append(old_hull, p);
  boost::geometry::convex_hull(old_hull, new_hull);

  double area_Pt = boost::geometry::area(new_hull);
  return(std::fabs(area_Pt - area));
}

double TreeSegment::compute_distance_to(PointXYZ &pt)
{
  point_t p(pt.x, pt.y);
  return boost::geometry::distance(p, convex_hull);
}

bool TreeSegment::add_point(PointXYZ &pt, double hmin)
{
  point_t p(pt.x, pt.y);

  if(boost::geometry::covered_by(p, convex_hull))
  {
    nbPoints++;
    if (pt.z < Zmin.z) Zmin = pt;
    return true;
  }

  if (pt.z < hmin)
    return false;

  nbPoints++;
  if (pt.z < Zmin.z) Zmin = pt;
  polygon old_hull(convex_hull);
  polygon new_hull;
  boost::geometry::append(old_hull, p);
  boost::geometry::convex_hull(old_hull, new_hull);
  convex_hull = new_hull;

  compute_area();
  return true;
}

point_t TreeSegment::get_apex()
{
  point_t p(Zmax.x, Zmax.y);
  return p;
}

void TreeSegment::compute_size_score()
{
  scoreS = 0;

  if (area == 0)
    return;

  // page 101 size criterion
  double H = Zmax.z;
  double D = nbPoints / area;
  double Th = k * D * std::log(H);

  if (nbPoints > Th)
    scoreS = 1;
  else
    scoreS = (nbPoints/Th);
}

void TreeSegment::compute_orientation_score()
{
  scoreO = 0;

  if (area == 0)
    return;

  size_t n = convex_hull.outer().size();

  point_t M;
  point_t G;
  point_t p;

  M = get_apex();
  boost::geometry::centroid(convex_hull, G);

  double D_MG = boost::geometry::distance(M, G);
  double D = 0;

  for (size_t j = 0 ; j < n ; j++)
  {
    p = convex_hull.outer().at(j);
    D += boost::geometry::distance(M, p);
  }

  D /= n;

  // page 101 Eq. 7 orientation criterion
  if (D_MG <= D/2.0)
    scoreO = 1.0 - 2.0*(D_MG/D);
}

void TreeSegment::compute_regularity_score()
{
  scoreR = 0;

  if (area == 0)
    return;

  std::vector<double> planimetricDist_MCH;

  point_t pt;
  point_t M = get_apex();

  for (size_t j = 0 ; j < convex_hull.outer().size() ; j++ )
  {
    pt = convex_hull.outer().at(j);
    planimetricDist_MCH.push_back(boost::geometry::distance(M, pt));
  }

  std::sort(planimetricDist_MCH.begin(), planimetricDist_MCH.end());
  int index_percentile95 = (int)(planimetricDist_MCH.size() * 0.95);

  double radius = planimetricDist_MCH[index_percentile95-1];

  //page 101 Eq. 8 regularity criterion
  scoreR = (area/(PI*radius*radius));
}

void TreeSegment::compute_circularity_score()
{
  scoreC = 0;

  if (area == 0)
    return;

  // calculate major and minor axes (A and B)
  std::pair<double, double> E = findEllipseParameters(convex_hull);
  double B = E.first > E.second ? E.first : E.second;
  double A = E.first < E.second ? E.first : E.second;

  // page 101 Eq.6 circularity criterion
  scoreC = (A/B);
}

void TreeSegment::compute_all_score()
{
  compute_size_score();
  compute_orientation_score();
  compute_circularity_score();
  compute_regularity_score();
  scoreGlobal = (scoreS + scoreO + scoreR + scoreC) / 4.0;
}

double TreeSegment::get_zmin() { return Zmin.z; }
double TreeSegment::get_zmax() { return Zmax.z; }
double TreeSegment::get_score() { return scoreGlobal; }

TreeSegment TreeSegment::merge(TreeSegment &t)
{
  TreeSegment newTree(this->k);

  polygon new_convex_hull;
  polygon union_of_convex_hulls(this->convex_hull);

  boost::geometry::append(union_of_convex_hulls, t.convex_hull.outer());
  boost::geometry::convex_hull(union_of_convex_hulls, new_convex_hull);

  PointXYZ new_Zmax = (this->Zmax.z > t.Zmax.z) ? this->Zmax : t.Zmax;

  newTree.convex_hull = new_convex_hull;
  newTree.Zmax = new_Zmax;
  newTree.nbPoints = this->nbPoints +  t.nbPoints;

  newTree.compute_area();
  newTree.compute_all_score();
  return(newTree);
}

std::pair<double, double> TreeSegment::findEllipseParameters(polygon &poly)
{
  size_t n = poly.outer().size();
  arma::dmat data(2, n);

  arma::dmat center = arma::zeros(1,2);
  for (size_t i = 0 ; i < n ; i++)
  {
    data(0, i) = poly.outer().at(i).get<0>();
    data(1, i) = poly.outer().at(i).get<1>();
  }

  unique(data);

  // means of each coordinates --> ellipse center
  for (size_t i = 0 ; i < n ; i++)
  {
    center(0,0) += data(0,i);
    center(0,1) += data(1,i);
  }
  center(0, 0) /= data.n_rows;
  center(0, 1) /= data.n_rows;

  // Covariance C of
  arma::dmat C = (data*data.t()) - center.t()*center;

  arma::mat PC = princomp(C);
  arma::mat data_PCA_2d = data.t() * PC;

  double max_x = max(data_PCA_2d.col(0));
  double max_y = max(data_PCA_2d.col(1));

  double min_x = min(data_PCA_2d.col(0));
  double min_y = min(data_PCA_2d.col(1));

  double half_axis_length1 = std::fabs(max_x - min_x);
  double half_axis_length2 = std::fabs(max_y - min_y);

  std::pair<double, double> L(half_axis_length1, half_axis_length2);

  return (L);
}

} //end Vega