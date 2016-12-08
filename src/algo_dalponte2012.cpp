#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

NumericVector filter_xx(NumericMatrix x, IntegerMatrix y)
{
  int nrow = y.nrow();
  NumericVector out(nrow);

  for(int i = 0 ; i < nrow ; i++)
    out(i) = x(y(i,0), y(i,1));

  return(out);
}

IntegerMatrix which_equal(IntegerMatrix mtx, double val)
{
  int l = mtx.nrow();
  int w = mtx.ncol();

  NumericVector x;
  NumericVector y;

  for(int i = 0 ; i < l ; i++)
  {
    for(int j = 0 ; j < w ; j++)
    {
      if(mtx(i,j) == val)
      {
        x.push_back(i);
        y.push_back(j);
      }
    }
  }

  IntegerMatrix m(x.length(), 2);
  m(_, 0) = x;
  m(_, 1) = y;

  return(m);
}

// [[Rcpp::export]]
IntegerMatrix itc_treetops(NumericMatrix Canopy, double searchWinSize)
{
  int l = Canopy.nrow();
  int w = Canopy.ncol();

  int r, k, minR, minC, maxR, maxC;
  int index = 1;

  double hws  = searchWinSize/2;
  int    fhws = floor(hws);
  int    chws = ceil(hws);

  NumericMatrix FIL;
  IntegerMatrix temp;

  IntegerMatrix Maxima(l, w);

  for (int r = chws ; r < l-chws ; r++)
  {
    for(int k = chws ; k <  w-chws ; k++)
    {
      minR = (r - fhws);
      minC = (k - fhws);
      maxR = (r + fhws);
      maxC = (k + fhws);

      FIL  = Canopy(Range(minR,maxR), Range(minC,maxC));
      temp = Maxima(Range(minR,maxR), Range(minC,maxC));

      if (FIL(chws-1,chws-1) == max(FIL) && max(temp) == 0 && max(FIL) != 0)
      {
        Maxima(r,k) = index;
        index++;
      }
    }
  }

  return(Maxima);
}

//[[Rcpp::export]]
IntegerMatrix itc_expandcrowns(NumericMatrix Canopy, IntegerMatrix Maxima, double TRESHSeed, double TRESHCrown, double DIST)
{
  bool gfil;
  bool it = true;

  int l = Canopy.nrow();
  int w = Canopy.ncol();
  int rr, kk, ind;

  double rvCrown, rvSeed;
  IntegerMatrix coordSeed, coordCrown;

  NumericMatrix filData(4, 3);

  IntegerMatrix OldCrowns  = clone(Maxima);
  IntegerMatrix Crowns     = clone(Maxima);
  IntegerMatrix Crownstemp = clone(Maxima);
  IntegerMatrix Check(Maxima.nrow(), Maxima.ncol());

  while (it)
  {
    it = false;

    for (int r = 1 ; r < l-1 ; r++)
    {
      for(int k = 1 ; k < w-1 ; k++)
      {
        if(Check(r, k) == 0 && Crowns(r, k) != 0)                       // Si le pixel est une couronne et qu'il n'a pas été testé déjà
        {
          ind = Crowns(r, k);                                           // On reccupère le numéro de couronne

          coordSeed  = which_equal(Maxima, ind);                        // Coordonnées du maximum local d'indice ind
          coordCrown = which_equal(Crowns, ind);                        // Coordonnées des pixels de cette couronne déjà attribués.

          rvSeed  = Canopy(coordSeed(0,0), coordSeed(0,1));             // Hauteur du maximum local
          rvCrown = mean(filter_xx(Canopy, coordCrown));                // Hauteur moyenne de la couronne

          filData(0, 0) = r - 1;                                        // Création d'une matrice contenant coordonnés des pixels en croix...
          filData(0, 1) = k;                                            // ... autour du pixel courant + la hauteur
          filData(0, 2) = Canopy(r - 1, k);
          filData(1, 0) = r;
          filData(1, 1) = k - 1;
          filData(1, 2) = Canopy(r, k - 1);
          filData(2, 0) = r;
          filData(2, 1) = k + 1;
          filData(2, 2) = Canopy(r, k + 1);
          filData(3, 0) = r + 1;
          filData(3, 1) = k;
          filData(3, 2) = Canopy(r + 1, k);

          // Test les 4 coordonnées pour trouver celles qui correspondend au test
          for(int i = 0 ; i < 4 ; i++)
          {
            rr   = filData(i, 0);
            kk   = filData(i, 1);
            gfil = (filData(i,2) != 0 && filData(i,2) > (rvSeed*TRESHSeed) && (filData(i,2) > (rvCrown*TRESHCrown)) && (filData(i,2) <= (rvSeed+(rvSeed*0.05))) && (abs(coordSeed(0,0)-filData(i,0)) < DIST) && (abs(coordSeed(0,1)-filData(i,1)) < DIST));

            if(gfil && Crowns(rr, kk) == 0 && Canopy(rr, kk) != 0)
            {
              Crownstemp(rr, kk) = Crowns(r, k);
              it = true;
            }
          }
        }
      }
    }

    std::copy( Crownstemp.begin(), Crownstemp.end(), Crowns.begin() );
    std::copy( OldCrowns.begin(), OldCrowns.end(), Check.begin() );
    std::copy( Crownstemp.begin(), Crownstemp.end(), OldCrowns.begin() );
  }

  return(Crowns);
}

