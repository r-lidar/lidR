

#include "TreeCollection.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

TreeCollection::TreeCollection()
{
  nbTree = 0;
}

TreeCollection::TreeCollection(TreeSegment &t)
{
  nbTree = 0;
  addTree(t);
}

TreeCollection::~TreeCollection(){}


void TreeCollection::addTree(TreeSegment &t)
{
  treeStorage.push_back(t);
  nbTree++;
}

void TreeCollection::calculateTreeScores(int k)
{
  for (unsigned int i = 0 ; i < nbTree ; i++)
  {
    treeStorage[i].compute_all_score(k);
  }
}

std::vector<TreeSegment> TreeCollection::search_trees_in_polygon(polygon poly)
{
  std::vector<TreeSegment> output;

  for (unsigned int n = 0 ; n < nbTree ; n++)
  {
    if (boost::geometry::covered_by(treeStorage[n].get_apex(), poly))
      output.push_back(treeStorage[n]);
  }

  return output;
}

int TreeCollection::searchID(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  // Here we found 2 or more potential trees for the current point. Some of these trees
  // may have less than 3 points. We have to adapt the search method because the rules on the
  // convex hull can't always be applied

  // To know which search method is required for this tree subset (distance or area evaluation)
  // scan the trees to identify if there is at least one of them with less than 2 points

  int searchMethod = 1;
  for (unsigned int j = 0 ; j < knnTreeID.size() ; j++)
  {
    if (treeStorage[knnTreeID[j]-1].nbPoints < 2)
    {
      searchMethod = 2;
      break;
    }
  }

  if(searchMethod == 1)
    return searchID_usingArea(knnTreeID, pointToSort);
  else
    return searchID_usingDist(knnTreeID, pointToSort);
}

int TreeCollection::searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  int resultID = knnTreeID[0];

  double area_increment = treeStorage[knnTreeID[0]-1].testArea(pointToSort);   // page 100 eq. 3

  for (unsigned int i = 1; i < knnTreeID.size(); i++ )
  {
    double area_increment2 = treeStorage[knnTreeID[i]-1].testArea(pointToSort);   // page 100 eq. 3
    if (area_increment > area_increment2)
    {
      area_increment = area_increment2;
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

int TreeCollection::searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  //Calcul de la premiere distance
  double distValueBis = 0;
  double distValue = treeStorage[knnTreeID[0]-1].testDist(pointToSort);
  int resultID = knnTreeID[0];

  // Comparaison avec les suivantes --> on attribue le point à l'arbre avec la distance la plus petite
  for (unsigned int i = 1 ; i < knnTreeID.size() ; i++ )
  {
    distValueBis = treeStorage[knnTreeID[i]-1].testDist( pointToSort );
    if (distValue > distValueBis)
    {
      distValue = distValueBis;
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

// JR incomplet.
void TreeCollection::remove_tree_with_less_than_3_points()
{
  unsigned int n = treeStorage.size();

  for (unsigned int i = (n-1) ; i > 0 ; i--)
  {
    if (treeStorage[i].convex_hull.outer().size() < 3)
    {
      treeStorage.erase(treeStorage.begin() + i);
      nbTree--;
    }
  }
}

// https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// derived from Rosetta Code
Rcpp::IntegerMatrix TreeCollection::createCombination(int N)
{
  Rcpp::IntegerMatrix res(std::pow(N-1,2)-(N-2), 2);   // pas optimal, taille matrice supérieure au besoin
  int ind = 0, ind2 = 0;

  std::string bitmask(2, 1); // K leading 1's
  bitmask.resize(N, 0); // N-K trailing 0's

  // print integers and permute bitmask
  do
  {
    for (int i = 0; i < N+1; i++) // [0..N-1] integers
    {
      if (bitmask[i]) res(ind,ind2++) = i+1;
    }

    ind++;
    ind2 = 0;
  } while (std::prev_permutation(bitmask.begin(), bitmask.end()));

  return(res);
}

Rcpp::List TreeCollection::to_R()
{
  int n = treeStorage.size();

  Rcpp::List TreeSegment;

  for (unsigned int i = 0 ; i < treeStorage.size() ; i++ )
  {
    double x = treeStorage[i].Zmax.x;
    double y = treeStorage[i].Zmax.y;

    point_t bary;
    boost::geometry::centroid(treeStorage[i].convex_hull, bary);

    Rcpp::NumericVector Apex = Rcpp::NumericVector::create(x,y);
    Rcpp::NumericVector Bary = Rcpp::NumericVector::create(bary.get<0>(),bary.get<1>());

    std::vector<point_t>& phull = treeStorage[i].convex_hull.outer();
    Rcpp::NumericMatrix rmat(phull.size(), 2);
    for (unsigned int j = 0; j < phull.size(); ++j)
    {
       const point_t& p = phull[j];
       rmat(j,0) = p.get<0>();
       rmat(j,1) = p.get<1>();
    }

    TreeSegment.push_back(Rcpp::List::create(Apex, Bary, rmat));
  }

  Rcpp::List output;
  output.push_back(TreeSegment);
  output.push_back(idTreeStorage);

  return(output);
}