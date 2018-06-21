#include "Point.h"
#include "QuadTree3D.h"
#include "TreeSegmentManager.h"
#include "TreeSegment.h"
#include "Progress.h"

typedef std::vector<PointXYZ> vpoint;
typedef QuadTree3D<PointXYZ> QuadTree;
typedef std::vector<ptrees::TreeSegment> vtreesegment;

ptrees::TreeSegmentManager PTrees_detection(vpoint&, int, double, QuadTree*, Progress&);
ptrees::TreeSegmentManager PTrees_segmentation(vpoint&, vpoint&, std::vector<int>&, double, QuadTree*, Progress&);

// [[Rcpp::export]]
Rcpp::List C_lastrees_ptrees(Rcpp::S4 las, std::vector<int> k_values, double hmin, int nmax, bool segmentation = true)
{
  // Initialization
  // ==============

  // Data conversion from las object to vector of PointXYZ
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector X = data["X"];
  Rcpp::NumericVector Y = data["Y"];
  Rcpp::NumericVector Z = data["Z"];

  unsigned int n = X.size();

  vpoint points(n);
  for (size_t i = 0 ; i < n ; i++)
    points[i] = PointXYZ(X[i], Y[i], Z[i], i);

  std::sort(points.begin(), points.end(), ZSort<PointXYZ>());
  std::sort(k_values.begin(), k_values.end(), std::greater<int>());

  // Creation of a QuadTree
  QuadTree *qtree;
  qtree = QuadTreeCreate(points);

  // Progress and check abort (user iteraction)
  unsigned int niter = k_values.size()*n;
  if (segmentation) niter += n;
  Progress p(niter, true);


  // Apply PTrees for the first scale k
  // ==================================

  // Initialize a first segmentation
  int k = k_values[0];
  ptrees::TreeSegmentManager its_reference = PTrees_detection(points, k, hmin, qtree, p);

  // If a single k is given we can't apply ptrees's selection rules. Return the unique segmentation
  if (k_values.size() == 1)
  {
    delete qtree;
    return its_reference.to_R();
  }

  // Apply PTrees for the next scales k
  // ==================================

  for (size_t i = 1 ; i < k_values.size() ; i++)
  {
    k = k_values[i];

    // New segmentation for the current scale k
    // ----------------------------------------

    ptrees::TreeSegmentManager its_new = PTrees_detection(points, k, hmin, qtree, p);
    ptrees::TreeSegmentManager its_temp(n, hmin); // To store the merge of the two segmentations.

    // Compare this new segmentation to the previous
    // one applying PTrees rules on each tree
    // ---------------------------------------------

    for (size_t j = 0 ; j < its_reference.size() ; j++)
    {
      ptrees::TreeSegment &tree_ref = its_reference.treeStorage[j];

      // Get the trees that belong into the reference tree
      vtreesegment trees_in_tree_ref = its_new.search_trees_in(tree_ref);
      int ntrees = trees_in_tree_ref.size();

      // If only one apex was found (page 103 Fig5 PartA-1)
      if(ntrees == 1)
      {
        // Comparison of score A and score D
        double score_ref = tree_ref.get_score();
        double score_compared = trees_in_tree_ref[0].get_score();

        if (score_ref > score_compared)
          its_temp.add_treesegment(tree_ref);
        else
          its_temp.add_treesegment(trees_in_tree_ref[0]);
      }
      // If two apices were found (page 103 Fig5 PartA-2)
      else if (ntrees == 2)
      {
        // Comparison of score B and score E+J
        double score_ref = tree_ref.get_score();
        double score_compared = ptrees::TreeSegmentManager::average_score(trees_in_tree_ref);

        if (score_ref > score_compared)
          its_temp.add_treesegment(tree_ref);
        else
        {
          its_temp.add_treesegment(trees_in_tree_ref[0]);
          its_temp.add_treesegment(trees_in_tree_ref[1]);
        }
      }
      // If more than two apices --> test of each combination
      else if (ntrees > 2)
      {
        double score_ref = tree_ref.get_score();
        double score_compared = 0;

        // The current best combination is initialized to all non merged trees
        vtreesegment &best_combination = trees_in_tree_ref;

        // If the number of trees to test is not too big, apply the rule
        // otherwise score_compared = 0 and the reference tree will be retained anyway.
        if (ntrees < nmax)
        {
          score_compared = ptrees::TreeSegmentManager::average_score(best_combination);

          // Create the different combinations
          std::vector< std::vector<int> > combinations = ptrees::TreeSegmentManager::createCombination(ntrees);

          // Loop throught all the possible combination (if any) and record the best score
          for (size_t k = 0 ; k < combinations.size(); k++)
          {
            // make merge the tree according the current combination
            ptrees::TreeSegment combined_tree = ptrees::TreeSegmentManager::build_combination(trees_in_tree_ref, combinations[k]);

            // get the other tree that were not merge
            vtreesegment other_trees = ptrees::TreeSegmentManager::get_non_combined_tree(trees_in_tree_ref, combinations[k]);

            // compute a total score
            double combined_tree_score = combined_tree.get_score();
            double other_tree_score = ptrees::TreeSegmentManager::average_score(other_trees);
            double final_score = (other_tree_score + combined_tree_score) / 2.0;

            // update scored_compared
            if (score_compared < final_score)
            {
              score_compared = final_score;
              best_combination.clear();
              best_combination = other_trees;
              best_combination.push_back(combined_tree);
            }
          }
        }

        // Compare the best combination to the reference tree
        // --------------------------------------------------

        // Keep the reference tree
        if (score_compared <= score_ref)
        {
          its_temp.add_treesegment(tree_ref);
        }
        // Keep the combination or/and all tree coming from the new tree collection
        else
        {
          for (size_t k = 0 ; k < best_combination.size(); k++)
            its_temp.add_treesegment(best_combination[k]);
        }
      }
    }

    if (p.check_abort())
    {
      delete qtree;
      p.exit();
    }

    std::swap(its_reference, its_temp);
  }

  // Are we segmenting or only finding trees?
  if (segmentation)
  {
    vpoint apices = its_reference.get_apices();
    its_reference = PTrees_segmentation(points, apices, k_values, hmin, qtree, p);
  }

  p.update(niter);

  delete qtree;
  return(its_reference.to_R());
}

ptrees::TreeSegmentManager PTrees_detection(vpoint &points, int k, double hmin, QuadTree *qtree, Progress &p)
{
  //   INITIALISATIONS
  // ======================

  unsigned int n = points.size();

  // Point are sorted, points 0 is belongs in the first tree
  PointXYZ u = points[0];
  ptrees::TreeSegment treeInit(u, k);
  ptrees::TreeSegmentManager trees(n, hmin);
  trees.add_treesegment(treeInit);

  //   ALGORITHM
  // ======================

  for (size_t i = 1 ; i <  n ; i++)
  {
    // Interaction with user
    if (p.check_abort())
    {
      delete qtree;
      p.exit();
    }

    p.increment();

    // Current point u
    u = points[i];

    // We reached the height limit, stop the tree detection
    if (u.z < hmin)
    {
      p.update(p.get_iter() + n - i);
      break;
    }

    // Search for the knn of u
    // -----------------------

    // knn search
    vpoint knn_points;
    qtree->knn_lookup3D(u, k, knn_points); // 'knn_points' contains the k neighbours + the current point

    // Remove points having a planimetric distance from u above threshold (page 100 eq. 1/2)
    vpoint filtered_knn_points;
    ptrees::TreeSegmentManager::planimetric_filter(knn_points, filtered_knn_points);

    // Search if some of these knn points were already classified
    // ----------------------------------------------------------

    std::vector<int> IDs = trees.search_neighbours_labels(filtered_knn_points);

    // Classify the current point
    // --------------------------

    // 1. If no classified points are found in the k neighbourhood this is a new tree (page 101 fig. 4B situation 1)
    if (IDs.empty())
    {
      ptrees::TreeSegment new_tree(u, k);
      trees.add_treesegment(new_tree);
    }
    // 2. If only one identified tree in the neighbourhood (page 101 fig. 4B situation 2)
    else if (IDs.size() == 1)
    {
      int ID = IDs[0]-1;
      trees.add_point_at(ID, u);
    }
    // 3. If several identified trees in the neighbourhood (page 101 fig. 4B situation 3)
    else
    {
      int ID = trees.search_best_match_tree_id(IDs, u)-1;

      // If point is to low it is a new tree (page 100 last paragraph)
      double diffHeight = std::fabs(trees.treeStorage[ID].get_zmin() - u.z);

      if (diffHeight <= 5)
      {
        trees.add_point_at(ID, u);
      }
      else
      {
        ptrees::TreeSegment new_tree(u, k);
        trees.add_treesegment(new_tree);
      }
    }
  }

  trees.remove_tree_with_less_than_3_points();
  trees.calculateTreeScores();
  return trees;
}

ptrees::TreeSegmentManager PTrees_segmentation(vpoint &points, vpoint &apices, std::vector<int> &k, double hmin, QuadTree *qtree, Progress &p)
{
  //   INITIALISATIONS
  // ======================

  PointXYZ u;
  unsigned int n = points.size();
  ptrees::TreeSegmentManager its(n, hmin);

  for (size_t i = 0 ; i < apices.size() ; i++)
  {
    ptrees::TreeSegment treeInit(apices[i], 0);
    its.add_treesegment(treeInit);
  }

  std::sort(k.begin(), k.end(), std::less<int>());

  //   ALGORITHM
  // ======================

  for (size_t i = 1 ; i < n ; i++)
  {
    if (p.check_abort())
    {
      delete qtree;
      p.exit();
    }

    p.increment();

    // current point
    u = points[i];

    // the point u is already labeled, it is a maxima, skip it.
    if (its.is_labeled(u)) continue;

    // Search for the knn of u
    // -----------------------

    size_t k_i = 0;
    std::vector<int> IDs;
    while(IDs.empty() && k_i < k.size())
    {
      // storage  corresponding points into result
      vpoint knn_points;
      qtree->knn_lookup3D(u, k[k_i], knn_points); // 'knn_points' contains the k neighbours + the current point

      // Removal of points having a planimetric distance from u above threshold T (page 100 eq. 1/2)
      vpoint filtered_knn_points;
      ptrees::TreeSegmentManager::planimetric_filter(knn_points, filtered_knn_points);

      // Search if some of these k points were already classified
      IDs = its.search_neighbours_labels(filtered_knn_points);
      k_i++;
    }

    // Classify the current point u
    // ----------------------------

    // 1. No classified points found in the search, do nothing
    if (IDs.empty())
    {

    }
    // 2. If only one identified tree in the neighbourhood (page 101 fig. 4B situation 2)
    else if (IDs.size() == 1)
    {
      its.add_point_at(IDs[0]-1, u);
    }
    // 3. If several identified trees in the neighbourhood (page 101 fig. 4B situation 3)
    else
    {
      double ID = its.search_best_match_tree_id(IDs, u)-1;

      // If point is to low it is a new tree (page 100 last paragraph)
      // JR: I think there is a missing a part here.
      double diffHeight = std::fabs(its.treeStorage[ID].get_zmin() - u.z);

      if (diffHeight <= 5)
        its.add_point_at(ID, u);
    }
  }

  its.remove_tree_with_less_than_3_points();
  its.calculateTreeScores();
  return its;
}
