#include "BoundingBox3D.h"
#include "Point.h"
#include "QuadTree3D.h"
#include "TreeCollection.h"
#include "TreeSegment.h"
#include "Progress.h"

using namespace Rcpp;

//========================================================================================
TreeCollection PTrees_segmentation(std::vector<PointXYZ> &points, int k, QuadTree3D<PointXYZ> *treeOI);
IntegerMatrix createCombination(int N);
double getScoreCombination(TreeSegment &t1, TreeSegment &t2, int k);
TreeSegment mergeTree(TreeSegment &t1, TreeSegment &t2, int k);
//========================================================================================


struct Increment {
  Increment() : m_value( 1 ) { }
  int operator()() { return m_value++; }
  int m_value;
};

//========================================================================================

// [[Rcpp::export]]
std::vector<int> C_lastrees_ptrees(S4 las, IntegerVector k_values)
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

  std::sort( points.begin(), points.end(), ZSortPointBis<PointXYZ>());   // Vector sorting by Z (from max to min)
  std::sort(k_values.begin(), k_values.end(), std::greater<int>());

  // Creation of a QuadTree
  QuadTree3D<PointXYZ> *treeOI;
  treeOI = QuadTreeCreate(points);

  Progress p(100, false);

  // Output
  std::vector<int> idResult;

  // Applying PTrees for the first k values
  // ============================================

  // Initialize a first segmentation
  Rcpp::Rcout << "k = "<< k_values[0] << std::endl;
  TreeCollection trees_kRef = PTrees_segmentation(points, k_values[0], treeOI);

  return trees_kRef.idTreeStorage;

  // If a single k is given we can't apply Vega's selection rules. Return the unique segmentation
  /*if (k_values.size() == 1)
  {
    delete treeOI;
    return trees_kRef.idTreeStorage;
  }

  // Applying PTrees for the next k values
  // ============================================

  for (unsigned int nb_k = 1 ; nb_k < k_values.size() ; nb_k++)
  {
    if (p.check_abort())
    {
      delete treeOI;
      p.exit();
    }

    // New segmentation for the current k
    // ----------------------------------

    Rcpp::Rcout << "k = "<< k_values[nb_k] << std::endl;
    TreeCollection<PointXYZ> trees_kToCompare = PTrees_segmentation(points, k_values[nb_k], treeOI);

    // Compare this segmentation to the previous one
    // ---------------------------------------------

    idResult.clear();
    idResult.assign(X.size(), 0);
    int index = 1; // Tree wil get this new ID

    std::vector<double> treeScores;
    std::vector<int> treeIDs;
    std::vector<PointXYZ> treeZmax;
    TreeCollection<PointXYZ> trees_kResult; // To store the merging of the two its.

    // Applying PTrees rules on each tree
    // ..................................

    for (unsigned int t = 0 ; t < trees_kRef.nbTree ; t++)
    {
      Tree<PointXYZ> &current_tree_ref = trees_kRef.treeStorage[t];

      treeScores.clear();
      treeIDs.clear();
      treeZmax.clear();

      // Get the convex hull of the tree
      polygon poly2D = current_tree_ref.get_convex_hull();

      // Search the apices that are in the convex hull of the current tree
      // JR: this should belong in its own method
      for (unsigned int n = 0 ; n < trees_kToCompare.nbTree ; n++)
      {
        // Apex search for each tree
        point_t p = trees_kToCompare.treeStorage[n].get_apex();

        // Apex included in polygon?
        if (boost::geometry::covered_by(p, poly2D) == true)
        {
          treeScores.push_back(trees_kToCompare.treeStorage[n].scoreGlobal);  // stockage du score global de l'arbre
          treeIDs.push_back(n);                                               // stockage de l'ID correspondant
          treeZmax.push_back(trees_kToCompare.treeStorage[n].Zmax);           // stockage de la hauteur max
        }
      }

      double scoreRef = current_tree_ref.scoreGlobal;

      // If only one apex was found (page 103 Fig5 PartA-1)
      if(treeIDs.size() == 1)
      {
        int keep_id = treeIDs[0];
        double scoreToCompare = trees_kToCompare.treeStorage[keep_id].scoreGlobal;

        // Comparison of score A and score D
        if (scoreRef > scoreToCompare)
        {
          trees_kResult.addTree(current_tree_ref);
          current_tree_ref.editIdResult(idResult, index);
        }
        else
        {
          trees_kResult.addTree( trees_kToCompare.treeStorage[keep_id] );
          trees_kToCompare.treeStorage[keep_id].editIdResult (idResult, index);
        }
      }
      // If two apices were found (page 103 Fig5 PartA-2)
      else if (treeIDs.size() == 2)
      {
        // Comparison of score B and score E+J
        if ( (treeScores[0] + treeScores[1])/2.0 > scoreRef)
        {
          trees_kResult.addTree(trees_kToCompare.treeStorage[treeIDs[0]]);
          trees_kResult.addTree(trees_kToCompare.treeStorage[treeIDs[1]]);

          trees_kToCompare.treeStorage[treeIDs[0]].editIdResult(idResult, index);
          trees_kToCompare.treeStorage[treeIDs[1]].editIdResult(idResult, index);
        }
        else
        {
          trees_kResult.addTree(current_tree_ref);
          current_tree_ref.editIdResult(idResult, index);
        }
      }
      // If more than two apices --> test of each combination
      else if (treeIDs.size() > 2)
      {
        // Find the best combination
        // -------------------------

        Rcpp::IntegerMatrix combination = createCombination(treeIDs.size());

        // Moyenne des scores des arbres à comparer
        double sumAllTrees = std::accumulate(treeScores.begin(), treeScores.end(), 0.0);
        double maxScore = sumAllTrees / treeScores.size();

        // !!! A commenter !!!
        std::vector<int> includedTreeForScore (2, INT16_MIN);
        std::vector<int> remainingTreeIDs(treeIDs.size(), 0);

        // Recherche de toutes les combinaisons d'arbres possibles et calcul de leurs scores
        // le meilleur score est conservé dans "maxScore" et sa combinaison associée est stockée dans "includedTreeForScore"
        for (int i = 0 ; i < combination.nrow() ; i++)
        {
          if (combination(i,0) != 0)
          {
            int nbTreeConsidered = 0;
            double calculatedScoreForCombination = 0;
            int tree1 = treeIDs[combination(i,0)-1];
            int tree2 = treeIDs[combination(i,1)-1];

            // Score of the combinated trees i.e. score of the new combined convex hull
            double scoreTreeCombination = getScoreCombination( trees_kToCompare.treeStorage[tree1], trees_kToCompare.treeStorage[tree2], nb_k );

            // Average score of non combined trees. (all the trees minus the combinated ones)
            remainingTreeIDs.clear();
            remainingTreeIDs.assign(treeIDs.size(), 0);
            std::generate(remainingTreeIDs.begin(), remainingTreeIDs.end(), Increment());
            remainingTreeIDs.erase(std::remove(remainingTreeIDs.begin(), remainingTreeIDs.end(), combination(i,0)), remainingTreeIDs.end());
            remainingTreeIDs.erase(std::remove(remainingTreeIDs.begin(), remainingTreeIDs.end(), combination(i,1)), remainingTreeIDs.end());

            double otherTreeScore = 0;
            for (int j = 0; j < remainingTreeIDs.size(); j++)
              otherTreeScore += treeScores[remainingTreeIDs[j]-1];

            double finalCombinationScore = ((otherTreeScore / remainingTreeIDs.size()) + scoreTreeCombination) / 2.0;

            if (maxScore < finalCombinationScore )
            {
              maxScore = finalCombinationScore;
              for (int k = 0; k < includedTreeForScore.size(); k++)
                includedTreeForScore[k] = combination(i,k);
            }
          }
        }

        // Compare the best combination to the reference tree (score)
        // ----------------------------------------------------------

        // Comparison between max score of possible combination (including mean score of all individual trees) and reference score
        if (maxScore <= scoreRef) //storage of tree coming from reference tree collection (the one which creates polygon for apex search)
        {
          trees_kResult.addTree( current_tree_ref );
          current_tree_ref.editIdResult (idResult, index);
        }
        else  // storage of best combination or all tree coming from tree collection to compare
        {
          // Si le maxScore est le même que celui de la moyenne de tous les scores des arbres à comparer
          // et qu'aucune combinaison n'a été stockée --> on stocke tous les arbres isolés
          if (maxScore == (sumAllTrees / treeScores.size()) && includedTreeForScore[0] == INT16_MIN )
          {
            for (unsigned int i = 0; i < treeIDs.size(); i++)
            {
              trees_kResult.addTree( trees_kToCompare.treeStorage[treeIDs[i]] );
              trees_kToCompare.treeStorage[treeIDs[i]].editIdResult(idResult, index);
            }
          }
          else  //sinon on stocke la combinaison (deux arbres fusionnés) et tous les autres arbres isolés
          {
            //Ajout de l'arbre fusionné (une des combinaisons de deux arbres isolés)
            int tree1 = treeIDs[includedTreeForScore[0]-1];
            int tree2 = treeIDs[includedTreeForScore[1]-1];
            Tree<PointXYZ> combinedTree;
            combinedTree = mergeTree(trees_kToCompare.treeStorage[tree1], trees_kToCompare.treeStorage[tree2], nb_k);

            trees_kResult.addTree( combinedTree );
            combinedTree.editIdResult (idResult, index);

            //ajout des arbres restants isolés
            std::vector<int> remainingTreeIDs(treeIDs.size());
            std::generate(remainingTreeIDs.begin(), remainingTreeIDs.end(), Increment());
            remainingTreeIDs.erase(std::remove(remainingTreeIDs.begin(), remainingTreeIDs.end(), includedTreeForScore[0]), remainingTreeIDs.end());
            remainingTreeIDs.erase(std::remove(remainingTreeIDs.begin(), remainingTreeIDs.end(), includedTreeForScore[1]), remainingTreeIDs.end());

            for (unsigned int i = 0; i < remainingTreeIDs.size(); i++)
            {
              int ID = remainingTreeIDs[i] - 1;
              trees_kResult.addTree( trees_kToCompare.treeStorage[treeIDs[ID]] );
              trees_kToCompare.treeStorage[treeIDs[ID]].editIdResult (idResult, index);
            }
          }
        }
      }

      for (unsigned int i = 0 ; i < trees_kResult.idTreeStorage.size() ; i++)
      {
        idResult.push_back(trees_kResult.idTreeStorage[i]);
      }
    }

    //std::swap(trees_kRef, trees_kResult);
  }

  delete treeOI;
  return(idResult);*/
}

//========================================================================================
//                               PTREES_SEGMENTATION
//========================================================================================
TreeCollection PTrees_segmentation(std::vector<PointXYZ> &points, int k, QuadTree3D<PointXYZ> *treeOI)
{
  // ======================
  //   INITIALISATIONS
  // ======================

  // First Zmax defines first tree in trees
  PointXYZ pointToSort = points[0];
  TreeSegment treeInit(pointToSort);
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
      TreeSegment newTree(pointToSort);
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
      double thresholdZ = 5;
      double resultID = trees.searchID(knnTreeID, pointToSort);

      // If point is to low it is a new tree (page 100 last paragraph)
      double diffHeight = std::fabs(trees.treeStorage[resultID-1].findZMin() - pointToSort.z);

      if (diffHeight <= thresholdZ)
      {
        trees.treeStorage[resultID-1].addPoint(pointToSort);
        idTree[pointToSort.id] = resultID;
      }
      else
      {
        TreeSegment newTree(pointToSort);
        trees.addTree(newTree);
        idTree[pointToSort.id] = trees.nbTree;
      }
    }
  }

  trees.remove_tree_with_less_than_3_points();
  trees.idTreeStorage = idTree;
  trees.calculateTreeScores(k);

  return trees;
}


//========================================================================================
//                              APPLY 2D FILTER FUNCTION
//========================================================================================

// Function that calculates 2D distance between one point (first one in subProfile) and its nearest neighbours
// returns only thoose under a calculated threshold


TreeSegment mergeTree(TreeSegment &t1, TreeSegment &t2, int k)
{
  TreeSegment newTree(t1);
  for (int i = 0; i < t2.nbPoints; i++)
    newTree.points.push_back(t2.points[i]);

  newTree.updateArea();
  newTree.getScore(k);
  newTree.nbPoints += t2.nbPoints;
  return(newTree);
}

//========================================================================================
double getScoreCombination(TreeSegment &t1, TreeSegment &t2, int k)
{
  TreeSegment combinedTrees;
  combinedTrees = mergeTree( t1, t2, k );
  return (combinedTrees.scoreGlobal);
}

//========================================================================================
//                              CREATE COMBINATION
//========================================================================================

// https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// derived from Rosetta Code
IntegerMatrix createCombination(int N)
{
  IntegerMatrix res(std::pow(N-1,2)-(N-2), 2);   //pas optimal
  int ind = 0, ind2 = 0;
  //for (int j = 1; j< N; j++)
  //{
  //ind = 0;
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
  //}
  return(res);
}




