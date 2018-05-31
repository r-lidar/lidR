#include "BoundingBox3D.h"
#include "Point.h"
#include "QuadTree3D.h"
#include "TreeCollection.h"
#include "TreeSegment.h"
#include "Progress.h"

using namespace Rcpp;

TreeCollection PTrees_segmentation(std::vector<PointXYZ> &points, int k, QuadTree3D<PointXYZ> *treeOI);

// [[Rcpp::export]]
List C_lastrees_ptrees(S4 las, IntegerVector k_values)
{
  // Initialization
  // ==============

  // Data conversion from las object to vector of PointXYZ
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  std::vector<PointXYZ> points(X.size());
  for (unsigned int i = 0 ; i < X.size() ; i++)
    points[i] = PointXYZ(X[i], Y[i], Z[i], i);

  std::sort(points.begin(), points.end(), ZSortPointBis<PointXYZ>());   // Vector sorting by Z (from max to min)
  std::sort(k_values.begin(), k_values.end(), std::greater<int>());

  // Creation of a QuadTree
  QuadTree3D<PointXYZ> *treeOI;
  treeOI = QuadTreeCreate(points);

  Progress p(100, false);

  // Applying PTrees for the first k values
  // ============================================

  // Initialize a first segmentation
  Rcpp::Rcout << "k = "<< k_values[0] << std::endl;
  TreeCollection its_reference = PTrees_segmentation(points, k_values[0], treeOI);

  // If a single k is given we can't apply Vega's selection rules. Return the unique segmentation
  if (k_values.size() == 1)
  {
    delete treeOI;
    return its_reference.to_R();
  }

  // Applying PTrees for the next k values
  // ============================================

  for (unsigned int nb_k = 1 ; nb_k < k_values.size() ; nb_k++)
  {
    Rcpp::Rcout << "k = "<< k_values[nb_k] << std::endl;

    if (p.check_abort())
    {
      delete treeOI;
      p.exit();
    }

    // New segmentation for the current k
    // ----------------------------------

    TreeCollection its_new = PTrees_segmentation(points, k_values[nb_k], treeOI);
    TreeCollection its_temp; // To store the merging of the two segmentation.

    // Compare this new segmentation to the previous one applying PTrees rules on each tree
    // -----------------------------------

    for (unsigned int t = 0 ; t < its_reference.nbTree ; t++)
    {
      TreeSegment &tree_ref = its_reference.treeStorage[t];

      // Get the trees that belong into the reference tree
      polygon &poly = tree_ref.convex_hull;
      std::vector<TreeSegment> trees_in_tree_ref = its_new.search_trees_in_polygon(poly);
      int ntrees = trees_in_tree_ref.size();

      // If only one apex was found (page 103 Fig5 PartA-1)
      if(ntrees == 1)
      {
        // Comparison of score A and score D
        double scoreRef = tree_ref.scoreGlobal;
        double scoreToCompare = trees_in_tree_ref[0].scoreGlobal;

        if (scoreRef > scoreToCompare)
          its_temp.addTree(tree_ref);
        else
          its_temp.addTree(trees_in_tree_ref[0]);
      }
      // If two apices were found (page 103 Fig5 PartA-2)
      else if (ntrees == 2)
      {
        // Comparison of score B and score E+J
        double scoreRef = tree_ref.scoreGlobal;
        double scoreToCompare = (trees_in_tree_ref[0].scoreGlobal + trees_in_tree_ref[0].scoreGlobal) / 2;

        if (scoreRef > scoreToCompare)
        {
          its_temp.addTree(tree_ref);
        }
        else
        {
          its_temp.addTree(trees_in_tree_ref[0]);
          its_temp.addTree(trees_in_tree_ref[1]);
        }
      }
      // If more than two apices --> test of each combination
      else if (ntrees > 2)
      {
        // Find the best combination
        // --------------------------

        // Set the different combinations
        std::vector< std::pair<int,int> > combination = TreeCollection::createCombination(ntrees);

        // The maximum score currently the average score of all the trees
        double maxScoreToCompare = 0;
        for (unsigned int ii = 0 ; ii < ntrees ; ii++)
          maxScoreToCompare += trees_in_tree_ref[ii].scoreGlobal;
        maxScoreToCompare /= ntrees;

        // Not a very elegant way
        std::pair<int, int> best_combination(INT16_MIN, INT16_MIN);

        // Loop throught all the possible combination and record the best score
        // The best score is stored in "maxScoreToCompare" and the best combination in "best_combination"
        for (unsigned int i = 0 ; i < combination.size(); i++)
        {
          // Score of the combinated trees i.e. score of the new combined convex hull
          int id1 = combination[i].first-1;
          int id2 = combination[i].second-1;
          TreeSegment &tree1 = trees_in_tree_ref[id1];
          TreeSegment &tree2 = trees_in_tree_ref[id2];
          double scoreTreeCombination = tree1.merge(tree2).scoreGlobal;

          // Average score of non combined trees (all the trees minus the combinated ones)
          double otherTreeScore = 0;
          for (unsigned int j = 0 ; j < ntrees ; j++)
          {
            if (j != id1 && j != id2)
              otherTreeScore += trees_in_tree_ref[j].scoreGlobal;
          }

          double finalCombinationScore = ((otherTreeScore / (ntrees-2)) + scoreTreeCombination) / 2.0;

          if (maxScoreToCompare < finalCombinationScore)
          {
            maxScoreToCompare = finalCombinationScore;
            best_combination.first = combination[i].first;
            best_combination.second = combination[i].second;
          }
        }

        // Compare the best combination to the reference tree (score)
        // ----------------------------------------------------------

        double scoreRef = tree_ref.scoreGlobal;

        // Keep the reference tree
        if (maxScoreToCompare <= scoreRef)
        {
          its_temp.addTree(tree_ref);
        }
        // Keep the combination or/and all tree coming from the new tree collection
        else
        {
          // No good combination, keep all the new trees
          if (best_combination.first == INT16_MIN)
          {
            for (unsigned int i = 0; i < ntrees; i++)
              its_temp.addTree(trees_in_tree_ref[i]);
          }
          // Keep the combinaison (two merged trees) and the other trees
          else
          {
            // Add the merged trees
            int id1 = best_combination.first-1;
            int id2 = best_combination.second-1;

            TreeSegment &tree1 = trees_in_tree_ref[id1];
            TreeSegment &tree2 = trees_in_tree_ref[id2];

            TreeSegment combinedTree = tree1.merge(tree2);

            its_temp.addTree(combinedTree);

            // Add the other trees
            for (unsigned int i = 0 ; i < ntrees; i++)
            {
              if (i != id1 & i != id2)
                its_temp.addTree( trees_in_tree_ref[i]);
            }
          }
        }
      }
    }

    std::swap(its_reference, its_temp);
  }

  delete treeOI;
  return(its_reference.to_R());
}

TreeCollection PTrees_segmentation(std::vector<PointXYZ> &points, int k, QuadTree3D<PointXYZ> *treeOI)
{
  // ======================
  //   INITIALISATIONS
  // ======================

  // First Zmax defines first tree in trees
  PointXYZ pointToSort = points[0];
  TreeSegment treeInit(pointToSort, k);
  TreeCollection trees(treeInit);

  // Creation of vector that stores relation between tree number and TreeCollection
  // Update for first point assignation
  std::vector<int> idTree(points.size(), 0);
  idTree[pointToSort.id] = trees.nbTree;

  std::vector<int> knnTreeID;
  std::vector<PointXYZ> knn_points;
  std::vector<PointXYZ> filtered_knn_points;

  // ======================
  //   ALGORITHM
  // ======================

  for (unsigned int i = 1 ; i <  points.size() ; i++)
  {
    // current point
    pointToSort = points[i];

    // Searching for k-nearest neighbours
    // ==================================

    // storage  corresponding points into result
    knn_points.clear();
    filtered_knn_points.clear();
    treeOI->knn_lookup3D(pointToSort, k, knn_points); // 'knn_points' contains the k neighbours + the current point

    // Removal of points having a planimetric distance from pointToSort above threshold T (page 100 eq. 1/2)
    TreeSegment::apply2DFilter(knn_points, filtered_knn_points);

    // Searching if some of these k points were already classified
    // ===========================================================

    knnTreeID.clear();
    knnTreeID.assign(filtered_knn_points.size(), 0);
    for (unsigned int n = 0 ; n < filtered_knn_points.size() ; n++)
    {
      knnTreeID[n] = idTree[filtered_knn_points[n].id];
    }

    // Removal of duplicates tree IDs and index value 0
    sort(knnTreeID.begin(), knnTreeID.end());
    knnTreeID.erase(unique(knnTreeID.begin(), knnTreeID.end()), knnTreeID.end());
    knnTreeID.erase(knnTreeID.begin());

    // Classify the current point
    // ==========================

    // 1. If no classified points are found in the k neighbourhood this is a new tree (page 101 fig. 4B situation 1)
    if (knnTreeID.empty())
    {
      TreeSegment newTree(pointToSort, k);
      trees.addTree(newTree);
      idTree[pointToSort.id] = trees.nbTree;
    }
    // 2. If only one identified tree in the neighbourhood (page 101 fig. 4B situation 2)
    else if (knnTreeID.size() == 1)
    {
      trees.treeStorage[knnTreeID[0]-1].addPoint(pointToSort);
      idTree[pointToSort.id] = knnTreeID[0];
    }
    // 3. If several identified trees in the neighbourhood (page 101 fig. 4B situation 3)
    else
    {
      double resultID = trees.searchID(knnTreeID, pointToSort);

      // If point is to low it is a new tree (page 100 last paragraph)
      double diffHeight = std::fabs(trees.treeStorage[resultID-1].Zmin.z - pointToSort.z);

      if (diffHeight <= 5)
      {
        trees.treeStorage[resultID-1].addPoint(pointToSort);
        idTree[pointToSort.id] = resultID;
      }
      else
      {
        TreeSegment newTree(pointToSort, k);
        trees.addTree(newTree);
        idTree[pointToSort.id] = trees.nbTree;
      }
    }
  }

  trees.remove_tree_with_less_than_3_points();
  trees.idTreeStorage = idTree;
  trees.calculateTreeScores();

  return trees;
}
