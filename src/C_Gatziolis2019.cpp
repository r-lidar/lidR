#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector filterTimeBlockPulses(DataFrame pulsedt)
{
  int n = pulsedt.nrow(); /* number of pulses */

  NumericVector pulseWeight = pulsedt["WT"];
  IntegerVector tBlock = pulsedt["TBLOCK"];
  IntegerVector flag(n);

  int minBlockWeight = 1E6, maxBlockWeight = -1E6;
  int minBlockRow = 0, maxBlockRow = 0;
  double tb = tBlock[0];
  int i = 0;

  while(i < n)
  {
    if (tBlock[i] == tb)
    {
      if (pulseWeight[i] < minBlockWeight)
      {
        minBlockWeight = pulseWeight[i];
        minBlockRow = i;
      }

      if(pulseWeight[i] > maxBlockWeight )
      {
        maxBlockWeight = pulseWeight[i];
        maxBlockRow = i;
      }
    }
    else
    {
      flag[minBlockRow] = 1;
      flag[maxBlockRow] = 1;
      minBlockWeight = 1E6;
      maxBlockWeight = -1E6;
      minBlockRow = i;
      maxBlockRow = i;
    }

    tb = tBlock[i];
    i++;
  }

  flag[minBlockRow] = 1;
  flag[maxBlockRow] = 1;

  return flag;
}

// [[Rcpp::export]]
DataFrame cmpCPA(DataFrame pulsedt)
{
  int n = pulsedt.nrow()/2; /* Number of pulse pairs */

  NumericVector XLast = pulsedt["XLAST"];
  NumericVector YLast = pulsedt["YLAST"];
  NumericVector ZLast = pulsedt["ZLAST"];
  NumericVector XFirst = pulsedt["XFIRST"];
  NumericVector YFirst = pulsedt["YFIRST"];
  NumericVector ZFirst = pulsedt["ZFIRST"];
  NumericVector pulseTime = pulsedt["T"];
  NumericVector pulseWeight = pulsedt["WT"];
  IntegerVector psi = pulsedt["PointSourceID"];

  NumericVector XCPA(n);
  NumericVector YCPA(n);
  NumericVector ZCPA(n);
  NumericVector CPADist(n);
  NumericVector CPATime(n);
  NumericVector CPAWeight(n);
  NumericVector PSI(n);

  double nearZeroValue = 1e-8;
  double l1[6], l2[6];
  double u[3], v[3], w[3], dP[3], cpa1[3], cpa2[3];
  double a, b, c, d, e, D, sc, tc;
  double t1, t2, w1, w2;
  int i, j;
  int psi1;

  for(i = 0; i < n; i++)
  {
    j = i*2; /* First pulse in pair */
    l1[0] = XLast[j];
    l1[1] = YLast[j];
    l1[2] = ZLast[j];
    l1[3] = XFirst[j];
    l1[4] = YFirst[j];
    l1[5] = ZFirst[j];
    t1    = pulseTime[j];
    w1    = fabs(pulseWeight[j]);
    psi1 = psi[j];

    j++; /* Second pulse in pair */
    l2[0] = XLast[j];
    l2[1] = YLast[j];
    l2[2] = ZLast[j];
    l2[3] = XFirst[j];
    l2[4] = YFirst[j];
    l2[5] = ZFirst[j];
    t2    = pulseTime[j];
    w2    = fabs(pulseWeight[j]);

    u[0] = l1[3] - l1[0];
    u[1] = l1[4] - l1[1];
    u[2] = l1[5] - l1[2];
    v[0] = l2[3] - l2[0];
    v[1] = l2[4] - l2[1];
    v[2] = l2[5] - l2[2];
    w[0] = l1[0] - l2[0];
    w[1] = l1[1] - l2[1];
    w[2] = l1[2] - l2[2];

    a = u[0] * u[0] + u[1] * u[1] + u[2] * u[2];
    b = u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
    c = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
    d = u[0] * w[0] + u[1] * w[1] + u[2] * w[2];
    e = v[0] * w[0] + v[1] * w[1] + v[2] * w[2];
    D = a * c - b * b;

    if( D < nearZeroValue )
    {
      sc = 0.0;

      if( b > c )
        tc = d / b;
      else
        tc = e / c;
    }
    else
    {
      sc = ( b * e - c * d ) / D;
      tc = ( a * e - b * d ) / D;
    }

    dP[0] = w[0] + sc * u[0] - tc * v[0];
    dP[1] = w[1] + sc * u[1] - tc * v[1];
    dP[2] = w[2] + sc * u[2] - tc * v[2];
    CPADist[i] = sqrt( dP[0] * dP[0] + dP[1] * dP[1] + dP[2] * dP[2] );

    cpa1[0] = l1[0] + u[0] * sc;
    cpa1[1] = l1[1] + u[1] * sc;
    cpa1[2] = l1[2] + u[2] * sc;
    cpa2[0] = l2[0] + v[0] * tc;
    cpa2[1] = l2[1] + v[1] * tc;
    cpa2[2] = l2[2] + v[2] * tc;

    XCPA[i] = ( cpa1[0] + cpa2[0] ) / 2.0;
    YCPA[i] = ( cpa1[1] + cpa2[1] ) / 2.0;
    ZCPA[i] = ( cpa1[2] + cpa2[2] ) / 2.0;

    CPATime[i]   = (t1 + t2) / 2.0;
    CPAWeight[i] = (w1 + w2) / 2.0;

    PSI[i] = psi1;
  }

  DataFrame output = DataFrame::create(
    _["X"] = XCPA,
    _["Y"] = YCPA,
    _["Z"] = ZCPA,
    _["DIST"] = CPADist,
    _["T"] = CPATime,
    _["WT"] = CPAWeight,
    _["PointSourceID"] = PSI);

  return output;
}
