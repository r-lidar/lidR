#include "LAS.h"
#include "SpatialIndex.h"

// From data.table https://github.com/Rdatatable/data.table/pull/7451/changes
#if R_VERSION < R_Version(4, 6, 0) || R_SVN_REVISION < 89077
# define BACKPORT_RESIZABLE_API
# define R_allocResizableVector(type, maxlen) R_allocResizableVector_(type, maxlen)
# define R_duplicateAsResizable(x) R_duplicateAsResizable_(x)
# define R_maxLength(x) R_maxLength_(x)
static inline R_xlen_t R_maxLength_(SEXP x) {
  return IS_GROWABLE(x) ? TRUELENGTH(x) : XLENGTH(x);
}
# define R_isResizable(x) R_isResizable_(x)
static inline bool R_isResizable_(SEXP x) {
  // IS_GROWABLE checks for XLENGTH < TRUELENGTH instead
  return (LEVELS(x) & 0x20) && XLENGTH(x) <= TRUELENGTH(x);
}
# define R_resizeVector(x, newlen) R_resizeVector_(x, newlen)
#endif


#ifdef BACKPORT_RESIZABLE_API
SEXP R_allocResizableVector_(SEXPTYPE type, R_xlen_t maxlen) {
  SEXP ret = Rf_allocVector(type, maxlen);
  SET_TRUELENGTH(ret, maxlen);
  SET_GROWABLE_BIT(ret);
  return ret;
}

void R_resizeVector_(SEXP x, R_xlen_t newlen) {
  if (!R_isResizable(x))
    Rcpp::stop("attempt to resize a non-resizable vector"); // # nocov
  if (newlen > XTRUELENGTH(x))
    Rcpp::stop("newlen exceeds maxlen"); // # nocov
  SETLENGTH(x, newlen);
}
#endif

List LAS::point_metrics(unsigned int k, double r, DataFrame data, int nalloc, SEXP call, SEXP env)
{
  // @k is the k of knn
  // @r is the radius
  // @data contains all the attributes of the LAS object so we are capable
  //       of copying the value in temporary memory
  // @n is the length of the vectors initially allocated to store the neighborhood
  // @call is the user-defined expression to apply on each neighborhood
  // @env is the environnement where Rf_eval eval the call

  // Are we searching the k nearest neiborhood or a sphere neighborhood or both?
  int mode = 0;
  if (k == 0 && r > 0)
    mode = 1;
  else if (k > 0 && r == 0)
    mode = 0;
  else if (k > 0 && r > 0)
    mode = 2;
  else
    Rcpp::stop("Internal error: invalid argument k or r");

  // Do we need to manage dynamic memory? Yes if not pure knn
  bool dynamic_memory_realloc = mode > 0;

  // Create an Rcpp object to handle the SEXP easily otherwise we have to use R's C API...
  Rcpp::Environment callenv = as<Rcpp::Environment>(env);

  // Retrieve the names of the attributes
  std::vector<std::string> names = as<std::vector<std::string> >(data.names());

  // Need some iterator to loop on the List columns
  Rcpp::List::iterator it1;
  Rcpp::List::iterator it2;
  std::vector<std::string>::iterator it3;

  // Need a physical List to handle a reference to the data of the environement 'env'.
  // Not mandatory stricly speaking but easier to handle this stuff with Rcpp than R's C API
  List proxy;

  // We populate the environement by creating new resizable vectors of size nalloc binded to
  // the original names in the LAS object + a shallow copy in the List 'proxy'.
  it3 = names.begin();
  for (it1 = data.begin() ; it1 != data.end() ; ++it1)
  {
    switch( TYPEOF(*it1) )
    {
    case REALSXP: {
      SEXP tmp = PROTECT(R_allocResizableVector(REALSXP, nalloc));
      proxy.push_back(tmp);
      callenv.assign(*it3, tmp);
      UNPROTECT(1);
      break;
    }
    case INTSXP: {
      SEXP tmp = PROTECT(R_allocResizableVector(INTSXP, nalloc));
      proxy.push_back(tmp);
      callenv.assign(*it3, tmp);
      UNPROTECT(1);
      break;
    }
    case LGLSXP: {
      SEXP tmp = PROTECT(R_allocResizableVector(LGLSXP, nalloc));
      proxy.push_back(tmp);
      callenv.assign(*it3, tmp);
      UNPROTECT(1);
      break;
    }
    default: {
      Rcpp::stop("Incompatible type encountered: integer, double and bool are the only supported types.");
    }
    }

    ++it3;
  }

  // Number of points actually processed considering the filter.
  // The output is allocated using this number
  int nprocessed = std::count(skip.begin(), skip.end(), true);
  List output(nprocessed);

  // Current index in the output
  int j = 0;

  // Construction of a spatial index to make the queries
  SpatialIndex tree(las, skip);
  Progress pb(npoints, "Metrics computation: ");

  // Error handling variables
  bool abort = false;
  int pOutError = 0;

  // This is the size of memory used to store the neighborhood
  unsigned int si = nalloc; // initially allocated
  unsigned int sc = si;     // current
  unsigned int sn = si;     // new

  if (!dynamic_memory_realloc && si != k) Rcpp::stop("Internal error: k elements should have been allocated.");

  //Rprintf("Memory allocated to store the neighborhood: %d\n", si);

  int protecti = 0;
  // Loop through each points
  for(unsigned int i = 0 ; i < npoints ; ++i)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true;
    pb.increment();
    if (!skip[i]) continue;

    std::vector<PointXYZ> pts;

    if (mode == 0)
    {
      // Query the knn neighborhood
      PointXYZ p(X[i], Y[i], Z[i]);
      tree.knn(p, k, pts);
      // No need to reallocate the memory because it is always of size k
    }
    else
    {
      if (mode == 1)
      {
        // Query the sphere neighborhood
        Sphere sp(X[i], Y[i], Z[i], r);
        tree.lookup(sp, pts);
      }
      else
      {
        // Query the knn + sphere limit
        PointXYZ p(X[i], Y[i], Z[i]);
        tree.knn(p, k, r, pts);
      }

      // This is the new size of the memory used to store the neighborhood
      sn = pts.size();

      // If we have found more points in the neighborhood than we have memory allocated: we need
      // to resize the memory. We resize x2 so we are sure that will occur only one or two times.
      if (sn > si)
      {
        si = (sn < 2*si) ? 2*si : sn;
        //Rprintf("Realloc from %d to %d at point %d because neigborhood of size %d\n", nalloc, si, i, sn);
        nalloc = si;
        sc = sn;
        proxy = List::create();

        it3 = names.begin();
        for (it1 = data.begin() ; it1 != data.end() ; ++it1)
        {
          switch( TYPEOF(*it1) )
          {
          case REALSXP: {
            SEXP tmp = PROTECT(R_allocResizableVector(REALSXP, nalloc)); protecti++;
            proxy.push_back(tmp);
            callenv.assign(*it3, tmp);
            R_resizeVector(tmp, sc);
            UNPROTECT(1);
            break;
          }
          case INTSXP: {
            SEXP tmp = PROTECT(R_allocResizableVector(INTSXP, nalloc)); protecti++;
            proxy.push_back(tmp);
            callenv.assign(*it3, tmp);
            R_resizeVector(tmp, sc);
            UNPROTECT(1);
            break;
          }
          case LGLSXP: {
            SEXP tmp = PROTECT(R_allocResizableVector(LGLSXP, nalloc)); protecti++;
            proxy.push_back(tmp);
            callenv.assign(*it3, tmp);
            R_resizeVector(tmp, sc);
            UNPROTECT(1);
            break;
          }
          default: {
            Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
          }
          }

          ++it3;
        }

      }
      // If we have found less points in the neighborhood than we have memory allocated: resize memory
      else
      {
        sc = sn;
        for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
          R_resizeVector(*it1, sc);
      }
    }


    // At this stage the environnment env should contains vectors named like into the LAS
    // object and these vector are longer or equal to the number of points in the neihborhood.
    // But at the R level they are of the good lenght because we used R_resizeVector.
    it2 = proxy.begin();
    for (it1 = data.begin() ; it1 != data.end() ; ++it1)
    {
      switch( TYPEOF(*it1) )
      {
      case REALSXP: {
        double* tmp1 = REAL(*it1);
        double* tmp2 = REAL(*it2);
        for(unsigned int p_idx = 0 ; p_idx < sc ; ++p_idx) tmp2[p_idx] = tmp1[pts[p_idx].id];
        break;
      }
      case INTSXP: {
        int* tmp1 = INTEGER(*it1);
        int* tmp2 = INTEGER(*it2);
        for(unsigned int p_idx = 0 ; p_idx < sc ; ++p_idx) tmp2[p_idx] = tmp1[pts[p_idx].id];
        break;
      }
      case LGLSXP: {
        int* tmp1 = LOGICAL(*it1);
        int* tmp2 = LOGICAL(*it2);
        for(unsigned int p_idx = 0 ; p_idx < sc ; ++p_idx) tmp2[p_idx] = tmp1[pts[p_idx].id];
        break;
      }
      default: {
        Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
      }
      }

      ++it2; ++it3;
    }

    output[j] = R_tryEvalSilent(call, env, &pOutError);

    if (pOutError == 1)
    {
      // Restore memory to allocated limits otherwise memory leak
      if (dynamic_memory_realloc)
      {
        for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
          R_resizeVector(*it1, si);
      }

      throw Rcpp::exception(R_curErrorBuf(), false);
    }

    j++;
  }

  // Restore memory to allocated limits otherwise memory leak
  if (dynamic_memory_realloc)
  {
    for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
      R_resizeVector(*it1, si);
  }

  UNPROTECT(protecti);

  return output;
}

/*
#ifdef WITHSETLENGTH
 List LAS::point_metrics(unsigned int k, double r, DataFrame data, int nalloc, SEXP call, SEXP env)
 {
 // @k is the k of knn
 // @r is the radius
 // @data contains all the attributes of the LAS object so we are capable
 //       of copying the value in temporary memory
 // @n is the length of the vectors initially allocated to store the neighborhood
 // @call is the user-defined expression to apply on each neighborhood
 // @env is the environnement where Rf_eval eval the call

 // Are we searching the k nearest neiborhood or a sphere neighborhood or both?
 int mode = 0;
 if (k == 0 && r > 0)
 mode = 1;
 else if (k > 0 && r == 0)
 mode = 0;
 else if (k > 0 && r > 0)
 mode = 2;
 else
 Rcpp::stop("Internal error: invalid argument k or r");

 // Do we need to manage dynamic memory? Yes if not pure knn
 bool dynamic_memory_realloc = mode > 0;

 // Create an Rcpp object to handle the SEXP easily otherwise we have to use R's C API...
 Rcpp::Environment callenv = as<Rcpp::Environment>(env);

 // Retrieve the names of the attributes
 std::vector<std::string> names = as<std::vector<std::string> >(data.names());

 // Need some iterator to loop on the List columns
 Rcpp::List::iterator it1;
 Rcpp::List::iterator it2;
 std::vector<std::string>::iterator it3;

 // Need a physical List to handle a reference to the data of the environement 'env'.
 // Not mandatory stricly speaking but easier to handle this stuff with Rcpp than R's C API
 List proxy;

 // We populate the environement by creating new vector of size nalloc binded to
 // the original names in the LAS object + a shallow copy in the List 'proxy'.
 it3 = names.begin();
 for (it1 = data.begin() ; it1 != data.end() ; ++it1)
 {
 switch( TYPEOF(*it1) )
 {
 case REALSXP: {
 Rcpp::NumericVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 break;
 }
 case INTSXP: {
 Rcpp::IntegerVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 break;
 }
 case LGLSXP: {
 Rcpp::LogicalVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 break;
 }
 default: {
 Rcpp::stop("Incompatible type encountered: integer, double and bool are the only supported types.");
 }
 }

 ++it3;
 }

 // Number of points actually processed considering the filter.
 // The output is allocated using this number
 int nprocessed = std::count(skip.begin(), skip.end(), true);
 List output(nprocessed);

 // Current index in the output
 int j = 0;

 // Construction of a spatial index to make the queries
 SpatialIndex tree(las, skip);
 Progress pb(npoints, "Metrics computation: ");

 // Error handling variables
 bool abort = false;
 int pOutError = 0;

 // This is the size of memory used to store the neighborhood
 unsigned int si = nalloc; // initially allocated
 unsigned int sc = si;     // current
 unsigned int sn = si;     // new

 if (!dynamic_memory_realloc && si != k) Rcpp::stop("Internal error: k elements should have been allocated.");

 //Rprintf("Memory allocated to store the neighborhood: %d\n", si);

 // Loop through each points
 for(unsigned int i = 0 ; i < npoints ; ++i)
 {
 if (abort) continue;
 if (pb.check_interrupt()) abort = true;
 pb.increment();
 if (!skip[i]) continue;

 std::vector<PointXYZ> pts;

 if (mode == 0)
 {
 // Query the knn neighborhood
 PointXYZ p(X[i], Y[i], Z[i]);
 tree.knn(p, k, pts);
 // No need to reallocate the memory because it is always of size k
 }
 else
 {
 if (mode == 1)
 {
 // Query the sphere neighborhood
 Sphere sp(X[i], Y[i], Z[i], r);
 tree.lookup(sp, pts);
 }
 else
 {
 // Query the knn + sphere limit
 PointXYZ p(X[i], Y[i], Z[i]);
 tree.knn(p, k, r, pts);
 }

 // This is the new size of the memory used to store the neighborhood
 sn = pts.size();

 // If we have found more points in the neighborhood than we have memory allocated: we need
 // to resize the memory. We resize x2 so we are sure that will occur only one or two times.
 if (sn > si)
 {
 si = (sn < 2*si) ? 2*si : sn;
 //Rprintf("Realloc from %d to %d at point %d because neigborhood of size %d\n", nalloc, si, i, sn);
 nalloc = si;
 sc = sn;
 proxy = List::create();

 it3 = names.begin();
 for (it1 = data.begin() ; it1 != data.end() ; ++it1)
 {
 switch( TYPEOF(*it1) )
 {
 case REALSXP: {
 Rcpp::NumericVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 SETLENGTH(wrap(tmp), sc);
 break;
 }
 case INTSXP: {
 Rcpp::IntegerVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 SETLENGTH(wrap(tmp), sc);
 break;
 }
 case LGLSXP: {
 Rcpp::LogicalVector tmp(nalloc);
 proxy.push_back(tmp);
 callenv.assign(*it3, tmp);
 SETLENGTH(wrap(tmp), sc);
 break;
 }
 default: {
 Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
 }
 }

 ++it3;
 }

 }
 // If we have found less points in the neighborhood than we have memory allocated: resize memory
 else
 {
 sc = sn;
 for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
 SETLENGTH(*it1, sc);
 }
 }


 // At this stage the environnment env should contains vectors named like into the LAS
 // object and these vector are longer or equal to the number of points in the neihborhood.
 // But at the R level they are of the good lenght because we used SETLENGTH.
 it2 = proxy.begin();
 for (it1 = data.begin() ; it1 != data.end() ; ++it1)
 {
 switch( TYPEOF(*it1) )
 {
 case REALSXP: {
 Rcpp::NumericVector tmp1 = Rcpp::as<Rcpp::NumericVector>(*it1);
 Rcpp::NumericVector tmp2 = Rcpp::as<Rcpp::NumericVector>(*it2);
 for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
 break;
 }
 case INTSXP: {
 Rcpp::IntegerVector tmp1 = Rcpp::as<Rcpp::IntegerVector>(*it1);
 Rcpp::IntegerVector tmp2 = Rcpp::as<Rcpp::IntegerVector>(*it2);
 for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
 break;
 }
 case LGLSXP: {
 Rcpp::LogicalVector tmp1 = Rcpp::as<Rcpp::LogicalVector>(*it1);
 Rcpp::LogicalVector tmp2 = Rcpp::as<Rcpp::LogicalVector>(*it2);
 for(unsigned int i = 0 ; i < sc ; ++i) tmp2[i] = tmp1[pts[i].id];
 break;
 }
 default: {
 Rcpp::stop("Incompatible SEXP encountered; only accepts DataFrame with REALSXPs, INTSXPs and LGLSXPs"); // # nocov
 }
 }

 ++it2; ++it3;
 }

 output[j] = R_tryEvalSilent(call, env, &pOutError);

 if (pOutError == 1)
 {
 // Restore the TRUELENGTH otherwise memory leak
 if (dynamic_memory_realloc)
 {
 for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
 SETLENGTH(*it1, si);
 }

 throw Rcpp::exception(R_curErrorBuf(), false);
 }

 j++;
 }

 // Restore the TRUELENGTH otherwise memory leak
 if (dynamic_memory_realloc)
 {
 for (it1 = proxy.begin() ; it1 != proxy.end() ; ++it1)
 SETLENGTH(*it1, si);
 }

 return output;
 }
#endif*/
