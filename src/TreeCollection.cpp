

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

void TreeCollection::calculateTreeScores()
{
  for (unsigned int i = 0 ; i < nbTree ; i++)
    treeStorage[i].compute_all_score();
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

TreeSegment TreeCollection::build_combination(std::vector<TreeSegment> &trees, std::vector<int> &combination)
{
  TreeSegment tree = trees[combination[0]];

  for (int i = 1 ; i < combination.size() ; i++)
  {
    int id = combination[i];
    TreeSegment tree2 = trees[id];
    tree = tree.merge(tree2);
  }

  return tree;
}

std::vector<TreeSegment> TreeCollection::get_non_combined_tree(std::vector<TreeSegment> &trees, std::vector<int> &combination)
{
  std::vector<TreeSegment> out_trees;

  for (int i = 0 ; i < trees.size() ; i++)
  {
    if(std::find(combination.begin(), combination.end(), i) == combination.end())
      out_trees.push_back(trees[i]);
  }

  return out_trees;
}

double TreeCollection::average_score(std::vector<TreeSegment>& trees)
{
  double score = 0;
  for (unsigned int i = 0 ; i < trees.size() ; i++)  score += trees[i].scoreGlobal;
  return score /= trees.size();
}


// https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// derived from Rosetta Code
std::vector< std::vector<int> > TreeCollection::createCombination(int N)
{
  std::vector< std::vector<int> > out;

  for (int j = 2; j < N+1; j++)
  {
    std::string bitmask(j, 1); // K leading 1's
    bitmask.resize(N, 0);      // N-K trailing 0's

    do
    {
      std::vector<int> v;

      for (int i = 0 ; i < N+1 ; i++)
        if (bitmask[i]) v.push_back(i);

        out.push_back(v);

    } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
  }

  return(out);
}

Rcpp::List TreeCollection::to_R()
{
  int n = treeStorage.size();

  Rcpp::List TreeSegment;

  for (unsigned int i = 0 ; i < treeStorage.size() ; i++ )
  {
    class TreeSegment tr = treeStorage[i];
    double x = tr.Zmax.x;
    double y = tr.Zmax.y;

    point_t bary;
    boost::geometry::centroid(tr.convex_hull, bary);

    Rcpp::NumericVector Apex = Rcpp::NumericVector::create(x,y);
    Rcpp::List Scores = Rcpp::List::create(Rcpp::Named("C") = tr.scoreC,
                                           Rcpp::Named("O") = tr.scoreO,
                                           Rcpp::Named("R") = tr.scoreR,
                                           Rcpp::Named("S") = tr.scoreS);
    Rcpp::NumericVector Bary = Rcpp::NumericVector::create(bary.get<0>(),bary.get<1>());

    std::vector<point_t>& phull = treeStorage[i].convex_hull.outer();
    Rcpp::NumericMatrix rmat(phull.size(), 2);
    for (unsigned int j = 0; j < phull.size(); ++j)
    {
       const point_t& p = phull[j];
       rmat(j,0) = p.get<0>();
       rmat(j,1) = p.get<1>();
    }

    TreeSegment.push_back(Rcpp::List::create(Rcpp::Named("Apex") = Apex,
                                             Rcpp::Named("Score") = Scores,
                                             Rcpp::Named("G") = Bary,
                                             Rcpp::Named("Hull") = rmat));
  }

  Rcpp::List output = Rcpp::List::create(Rcpp::Named("TreeSegment") = TreeSegment,
                                         Rcpp::Named("treeID") = idTreeStorage);

  return(output);
}