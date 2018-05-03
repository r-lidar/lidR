#include "Point.h"
#include "QuadTree3D.h"
#include "BoundingBox3D.h"
#include "TreeCollection.h"
#include "Tree.h"

#include <Rcpp.h>
using namespace Rcpp;

typedef boost::geometry::model::point<double, 3, boost::geometry::cs::cartesian> polygonPoint;

template<typename T> void apply2DFilter( std::vector<T> &subProfile, std::vector<T> &subProfileSubset );


template<typename T> void searchID_usingArea( TreeCollection<T> *trees, std::vector<int> &knnTreeID, T &pointToSort, int &resultID, double &areaValue )
{
  double areaValueBis = 0;
  areaValue = trees->treeStorage[knnTreeID[0]-1].testArea( pointToSort );
  for ( int i = 1; i < knnTreeID.size(); i++ )
  {
    areaValueBis = trees->treeStorage[knnTreeID[i]-1].testArea( pointToSort );
    if (areaValue > areaValueBis)
    {
      areaValue = areaValueBis;
      resultID = knnTreeID[i];
    }
  }
}

template<typename T> void searchID_usingDist( TreeCollection<T> *trees, std::vector<int> &knnTreeID, T &pointToSort, int &resultID, double &distValue )
{
  double distValueBis = 0;
  distValue = trees->treeStorage[knnTreeID[0]-1].testDist( pointToSort );
  for ( int i = 1; i < knnTreeID.size(); i++ )
  {
    distValueBis = trees->treeStorage[knnTreeID[i]-1].testDist( pointToSort );
    if (distValue > distValueBis)
    {
      distValue = distValueBis;
      resultID = knnTreeID[i];
    }
  }
}

// [[Rcpp::export]]
std::vector<int> test_PTrees( S4 las, int k )
{
  //Data conversion from las object to vector of PointXYZ
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  std::vector<PointXYZ> points(X.size());
  for (int i = 0 ; i < X.size() ; i++)
    points[i] = PointXYZ(X[i], Y[i], Z[i], i);

  //Vector sorting by Z (from max to min)
  std::sort( points.begin(), points.end(), ZSortPointBis<PointXYZ>() );

  //First Zmax defines first tree in trees
  PointXYZ pointToSort = points[0];
  Tree<PointXYZ> treeInit( pointToSort );
  TreeCollection<PointXYZ> trees ( treeInit );

  //Creation of vector that stores relation between tree number and TreeCollection
  //Update for first point assignation
  std::vector<int> idTree( X.size(), 0 );
  idTree[pointToSort.id] = trees.nbTree;

  //Creation of a QuadTree
  QuadTree3D<PointXYZ> *treeOI;
  treeOI = QuadTreeCreate(points);

  //Setting of resolution --> put as argument?
  PointXYZ reso = PointXYZ(0.01,0.01,0.01);


  std::vector<int> knnTreeID;
  std::vector<PointXYZ> result, filteredResult;
  BoundingBox3D<PointXYZ> bbox;

  int limit = points.size();
  for (int i = 1; i < limit; i ++ )
  {
    Rcpp::Rcout << "i = " << i << std::endl;
    //Initialisation of point to assign in trees
    pointToSort = points[i];

    //Searching for k-nearest neighbours and storage of corresponding points into result
    bbox = BoundingBox3D<PointXYZ> (pointToSort, reso);
    result.clear();
    filteredResult.clear();
    treeOI->knn_lookup3D( pointToSort, k, result );

    //Removal of points having a planimetric distance from pointToSort above threshold T
    apply2DFilter(result, filteredResult);

    //Searching if these k points were already classified in idTree using ID number in result
    knnTreeID.clear();
    knnTreeID.assign( filteredResult.size(), 0 );
    for (int n = 0; n < k; n++ )
    {
      knnTreeID[n] = idTree[filteredResult[n].id];
    }

    //Removal of duplicates tree IDs and index value 0
    sort( knnTreeID.begin(), knnTreeID.end() );
    knnTreeID.erase( unique( knnTreeID.begin(), knnTreeID.end() ), knnTreeID.end() );
    knnTreeID.erase(knnTreeID.begin());


    //Three possibilities for next classification:
    //1) If no classified points in the neighbourhood
    if ( knnTreeID.empty() == TRUE )
    {
      Rcpp::Rcout << "knnTreeID = " << 1 << std::endl;
      Tree<PointXYZ> newTree( pointToSort );
      trees.addTree( newTree );
      idTree[pointToSort.id] = trees.nbTree;
    }
    //2) If only one identified tree in the neighbourhood
    else if ( knnTreeID.size() == 1)
    {
      Rcpp::Rcout << "knnTreeID = " << 2 << std::endl;
      trees.updateTree( knnTreeID[0], pointToSort );
      idTree[pointToSort.id] = knnTreeID[0];
    }
    //3) If several identified trees in the neighbourhood
    else
    {
      Rcpp::Rcout << "knnTreeID = " << 3 << std::endl;
      //To know which search Method is required for this tree subset (distance or area evaluation)
      //scan of tree numbers to identify if there is at least one with less than 2 points
      int searchMethod = 1;
      for ( int i = 0; i < knnTreeID.size(); i++ )
      {
        if ( trees.treeStorage[knnTreeID[i]-1].nbPoints < 2 )
          searchMethod = 2;
      }

      //Depending on previous result, selection of adapted searchMethod
      int resultID = knnTreeID[0];
      double areaValue = 0, distValue = 0;
      switch(searchMethod)
      {
      case 1: Rcpp::Rcout << "knnTreeID = " << 4 << std::endl;
        searchID_usingArea( &trees, knnTreeID, pointToSort, resultID, areaValue );
        //Association of pointToSort to best tree result
        trees.treeStorage[resultID-1].addPoint( pointToSort, areaValue );
        idTree[pointToSort.id] = resultID;
        break;

      case 2: Rcpp::Rcout << "knnTreeID = " << 5 << std::endl;
        searchID_usingDist( &trees, knnTreeID, pointToSort, resultID, distValue );
        //Association of pointToSort to best tree result
        trees.treeStorage[resultID-1].addPoint_dist( pointToSort, distValue );
        idTree[pointToSort.id] = resultID;
        break;
      }

      //attention fixer un seuil à 5m!!
    }

  }
/*
//========================================================================================
  int j = limit;
  Rcpp::Rcout << "i = " << j << std::endl;
  //Initialisation of point to assign in trees
  pointToSort = points[j];

  //Searching for k-nearest neighbours and storage of corresponding points into result
  bbox = BoundingBox3D<PointXYZ> (pointToSort, reso);
  result.clear();
  filteredResult.clear();
  treeOI->knn_lookup3D( pointToSort, k, result );

  //Removal of points having a planimetric distance from pointToSort above threshold T
  apply2DFilter(result, filteredResult);

  //Searching if these k points were already classified in idTree using ID number in result
  knnTreeID.clear();
  knnTreeID.assign( filteredResult.size(), 0 );
  for (int n = 0; n < k; n++ )
  {
  knnTreeID[n] = idTree[filteredResult[n].id];
  }

  //Removal of duplicates tree IDs and index value 0
  sort( knnTreeID.begin(), knnTreeID.end() );
  knnTreeID.erase( unique( knnTreeID.begin(), knnTreeID.end() ), knnTreeID.end() );
  knnTreeID.erase(knnTreeID.begin());

  double areaValue = 0;
  //Three possibilities for next classification:
  //1) If no classified points in the neighbourhood
  if ( knnTreeID.empty() == TRUE )
  {
  Rcpp::Rcout << "knnTreeID = " << 1 << std::endl;
  Tree<PointXYZ> newTree( pointToSort );
  trees.addTree( newTree );
  idTree[pointToSort.id] = trees.nbTree;
  }
  //2) If only one identified tree in the neighbourhood
  else if ( knnTreeID.size() == 1)
  {
  Rcpp::Rcout << "knnTreeID = " << 2 << std::endl;
  trees.updateTree( knnTreeID[0], pointToSort );
  idTree[pointToSort.id] = knnTreeID[0];
  }
  //3) If several identified trees in the neighbourhood
  else
  {
    Rcpp::Rcout << "knnTreeID = " << 3 << std::endl;
    //To know which search Method is required for this tree subset (distance or area evaluation)
    //scan of tree numbers to identify if there is at least one with less than 2 points
    int searchMethod = 1;
    for ( int i = 0; i < knnTreeID.size(); i++ )
    {
      if ( trees.treeStorage[knnTreeID[i]-1].nbPoints < 2 )
        searchMethod = 2;
    }

    //Depending on previous result, selection of adapted searchMethod
    int resultID = knnTreeID[0];
    //double areaValue = 0;
    double distValue = 0;

    switch(searchMethod)
    {
    case 1: Rcpp::Rcout << "knnTreeID = " << 4 << std::endl;
      //searchID_usingArea( trees, knnTreeID, pointToSort, resultID, areaValue );
      searchID_usingArea( &trees, knnTreeID, pointToSort, resultID, areaValue );
      //Association of pointToSort to best tree result
      trees.treeStorage[resultID-1].addPoint( pointToSort, areaValue );
      idTree[pointToSort.id] = resultID;
      break;

    case 2: Rcpp::Rcout << "knnTreeID = " << 5 << std::endl;
      //searchID_usingDist( trees, knnTreeID, pointToSort, resultID, distValue );
      //Association of pointToSort to best tree result
      //trees.treeStorage[resultID-1].addPoint_dist( pointToSort, distValue );
      idTree[pointToSort.id] = resultID;
      break;
    }
  }*/

//========================================================================================

/*
NumericMatrix output(1, 5);
for (int i = 0; i < 1; i++ )
{
  output(i, 0) = pointToSort.x;
  output(i, 1) = pointToSort.y;
  output(i, 2) = pointToSort.z;
  output(i, 3) = pointToSort.id;
}*/

/*
 NumericMatrix output(pointsForPoly.size(), 3);
 for (int i = 0; i < pointsForPoly.size(); i++ )
 {
 output(i, 0) = pointsForPoly[i].get<0>();
 output(i, 1) = pointsForPoly[i].get<1>();
 output(i, 2) = pointsForPoly[i].get<2>();
 }
*/

/*
  int ind1 = 0, ind2 = 0;
NumericMatrix output(100, 5);
  for (int j = 0; j <trees.treeStorage.size(); j++ )
  {
    for (int i = 0; i < trees.treeStorage[j].points.size(); i++ )
    {
      output(ind2 +i, 0) = trees.treeStorage[j].points[i].x;
      output(ind2 +i, 1) = trees.treeStorage[j].points[i].y;
      output(ind2 +i, 2) = trees.treeStorage[j].points[i].z;
      output(ind2 +i, 3) = trees.treeStorage[j].points[i].id;
      output(ind2 +i, 4) = j+1;
      ind1++;
    }
    ind2 += ind1;
    ind1 = 0;
  }
*/



return(idTree);

}



//========================================================================================
//                              APPLY 2D FILTER FUNCTION
//========================================================================================
//Function that calculates 2D distance between one point (first one in subProfile) and its nearest neighbours
//returns only thoose under a calculated threshold
template<typename T> void apply2DFilter( std::vector<T> &subProfile, std::vector<T> &subProfileSubset )
{
  double meanValueForThreshold = 0;
  std::vector<double> dist;
  double stdValueForThreshold = 0;
  double val = 0;
  //Euclidian Distance calculation in 2D for all neighbours regarding the reference point (storage in Z value)
  for ( int i = 1; i < subProfile.size(); i++ )
  {
    val = euclidianDistance2D_inZ( subProfile[0], subProfile[i] );
    meanValueForThreshold += val;
    dist.push_back( val );
  }

  //--------------------------------------------------------------------------------------
  //Threshold definition (page 100 'segmentation principles')
  //defined as the mean plus twice the std of the planimetric distances of a subset of points
  meanValueForThreshold /= (subProfile.size() - 1);
  double sum = 0;
  for(int i = 0; i<dist.size(); i++)
    sum += (dist[i]-meanValueForThreshold) * (dist[i]-meanValueForThreshold);

  stdValueForThreshold = sqrt( sum / dist.size() );

  double threshold = meanValueForThreshold + 2*stdValueForThreshold;
  //--------------------------------------------------------------------------------------
  //Keeping all points below threshold and storage of their IDs
  int i = 0, keep = 0;
  subProfileSubset.push_back(subProfile[0]);
  while ( (i < subProfile.size()-1) && (dist[i] <= threshold) )
  {
    subProfileSubset.push_back( subProfile[i+1] );
    i++;
  }

}