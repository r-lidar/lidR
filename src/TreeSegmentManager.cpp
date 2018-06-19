

#include "TreeSegmentManager.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

namespace ptrees
{

TreeSegmentManager::TreeSegmentManager(unsigned int npoints, double hmin)
{
  this->hmin = hmin;
  points_labels.assign(npoints, 0);
}

TreeSegmentManager::~TreeSegmentManager(){}

size_t TreeSegmentManager::size()
{
  return treeStorage.size();
}

bool TreeSegmentManager::is_labeled(const PointXYZ &u)
{
  return points_labels[u.id] != 0;
}

void TreeSegmentManager::add_treesegment(TreeSegment &t)
{
  treeStorage.push_back(t);
  points_labels[t.Zmax.id] = size();
}

void TreeSegmentManager::add_point_at(unsigned int p, PointXYZ &u)
{
  if(treeStorage[p].add_point(u, hmin))
    points_labels[u.id] = p+1;
}

void TreeSegmentManager::calculateTreeScores()
{
  for (unsigned int i = 0 ; i < this->size() ; i++)
    treeStorage[i].compute_all_score();
}

std::vector<PointXYZ> TreeSegmentManager::get_apices()
{
  std::vector<PointXYZ> apices;
  for (size_t i = 0 ; i < this->size() ; i++)
    apices.push_back(treeStorage[i].Zmax);

  return apices;
}

std::vector<TreeSegment> TreeSegmentManager::search_trees_in(const TreeSegment &ref)
{
  std::vector<TreeSegment> output;
  for (unsigned int n = 0 ; n < this->size() ; n++)
  {
    if (boost::geometry::covered_by(treeStorage[n].get_apex(), ref.convex_hull))
      output.push_back(treeStorage[n]);
  }

  return output;
}

int TreeSegmentManager::search_best_match_tree_id(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
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


std::vector<int> TreeSegmentManager::search_neighbours_labels(std::vector<PointXYZ>& filtered_knn_points)
{
  std::vector<int> knnTreeID(filtered_knn_points.size());
  for (size_t i = 0 ; i < filtered_knn_points.size() ; i++)
    knnTreeID[i] = points_labels[filtered_knn_points[i].id];

  // Removal of duplicates tree IDs and index value 0
  sort(knnTreeID.begin(), knnTreeID.end());
  knnTreeID.erase(unique(knnTreeID.begin(), knnTreeID.end()), knnTreeID.end());
  knnTreeID.erase(knnTreeID.begin());
  return knnTreeID;
}

int TreeSegmentManager::searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  int resultID = knnTreeID[0];

  double area_increment = treeStorage[knnTreeID[0]-1].compute_area_increment(pointToSort);   // page 100 eq. 3

  for (unsigned int i = 1; i < knnTreeID.size(); i++ )
  {
    double area_increment2 = treeStorage[knnTreeID[i]-1].compute_area_increment(pointToSort);   // page 100 eq. 3
    if (area_increment > area_increment2)
    {
      area_increment = area_increment2;
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

int TreeSegmentManager::searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  //Calcul de la premiere distance
  double distValueBis = 0;
  double distValue = treeStorage[knnTreeID[0]-1].compute_distance_to(pointToSort);
  int resultID = knnTreeID[0];

  // Comparaison avec les suivantes --> on attribue le point à l'arbre avec la distance la plus petite
  for (unsigned int i = 1 ; i < knnTreeID.size() ; i++ )
  {
    distValueBis = treeStorage[knnTreeID[i]-1].compute_distance_to(pointToSort);
    if (distValue > distValueBis)
    {
      distValue = distValueBis;
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

void TreeSegmentManager::remove_tree_with_less_than_3_points()
{
  size_t n = treeStorage.size();
  for (size_t i = (n-1) ; i > 0 ; i--)
  {
    if (treeStorage[i].convex_hull.outer().size() < 3)
    {
      treeStorage.erase(treeStorage.begin() + i);
      std::replace(points_labels.begin(), points_labels.end(), int(i+1), 0);
    }
  }
}

TreeSegment TreeSegmentManager::build_combination(std::vector<TreeSegment> &trees, std::vector<int> &combination)
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

std::vector<TreeSegment> TreeSegmentManager::get_non_combined_tree(std::vector<TreeSegment> &trees, std::vector<int> &combination)
{
  std::vector<TreeSegment> out_trees;

  for (int i = 0 ; i < trees.size() ; i++)
  {
    if(std::find(combination.begin(), combination.end(), i) == combination.end())
      out_trees.push_back(trees[i]);
  }

  return out_trees;
}

double TreeSegmentManager::average_score(std::vector<TreeSegment>& trees)
{
  double score = 0;
  for (unsigned int i = 0 ; i < trees.size() ; i++)  score += trees[i].scoreGlobal;
  return score /= trees.size();
}


// https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// derived from Rosetta Code
std::vector< std::vector<int> > TreeSegmentManager::createCombination(int N)
{
  if (N > 6)
  {
    Rcpp::Rcerr << "Warning: " << N << " trees found lead to " << std::pow(2,N)-N-1 << " possible combinations." << std::endl;
  }

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

void TreeSegmentManager::planimetric_filter(std::vector<PointXYZ> &subProfile, std::vector<PointXYZ> &subProfileSubset )
{
  double meanValueForThreshold = 0;
  std::vector<double> dist;
  double stdValueForThreshold = 0;
  double val = 0;
  // Euclidian Distance calculation in 2D for all neighbours regarding the reference point (storage in Z value)
  for (unsigned int i = 1; i < subProfile.size(); i++ )
  {
    val = euclidianDistance2D_inZ( subProfile[0], subProfile[i] );
    meanValueForThreshold += val;
    dist.push_back( val );
  }

  //--------------------------------------------------------------------------------------
  // Threshold definition (page 100 'segmentation principles')
  // defined as the mean plus twice the std of the planimetric distances of a subset of points
  meanValueForThreshold /= (double)(subProfile.size() - 1);
  double sum = 0;
  for(unsigned int i = 0; i < dist.size(); i++)
    sum += (dist[i]-meanValueForThreshold) * (dist[i]-meanValueForThreshold);

  stdValueForThreshold = std::sqrt( sum / (double) dist.size() );

  double threshold = meanValueForThreshold + 2*stdValueForThreshold;
  //--------------------------------------------------------------------------------------
  // Keeping all points below threshold and storage of their IDs
  int i = 0, keep = 0;
  subProfileSubset.push_back(subProfile[0]);
  while ( (i < subProfile.size()-1) && (dist[i] <= threshold) )
  {
    subProfileSubset.push_back( subProfile[i+1] );
    i++;
  }
}

Rcpp::List TreeSegmentManager::to_R()
{
  int n = treeStorage.size();

  std::replace(points_labels.begin(), points_labels.end(), 0, NA_INTEGER);

  Rcpp::List TreeSegment;

  Rcpp::NumericMatrix Apex(treeStorage.size(), 3);

  for (unsigned int i = 0 ; i < n ; i++ )
  {
    class TreeSegment tr = treeStorage[i];
    double x = tr.Zmax.x;
    double y = tr.Zmax.y;
    double z = tr.Zmax.z;

    point_t bary;

  try
  {
    boost::geometry::centroid(tr.convex_hull, bary);
  }
  catch(std::exception const&  ex)
  {
    Rcpp::Rcout << i << "/" << n << std::endl;
    Rcpp::Rcout << ex.what() << std::endl;
    Rcpp::Rcout << tr.convex_hull.outer().size() << std::endl;
    Rcpp::Rcout << tr.nbPoints << std::endl;
    throw;
  }


    Apex(i, 0) = x;
    Apex(i, 1) = y;
    Apex(i, 2) = z;

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

    TreeSegment.push_back(Rcpp::List::create(Rcpp::Named("Score") = Scores,
                                             Rcpp::Named("G") = Bary,
                                             Rcpp::Named("Hull") = rmat));
  }

  Rcpp::List output = Rcpp::List::create(Rcpp::Named("Apices") = Apex,
                                         Rcpp::Named("TreeSegment") = TreeSegment,
                                         Rcpp::Named("treeID") = points_labels);

  return(output);
}

} //end Vega