#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _lidR_algo_li2012(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_fast_countbelow(SEXP, SEXP);
extern SEXP _lidR_fast_countequal(SEXP, SEXP);
extern SEXP _lidR_fast_countover(SEXP, SEXP);
extern SEXP _lidR_fast_table(SEXP, SEXP);
extern SEXP _lidR_fast_extract(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_tinfo(SEXP, SEXP);
extern SEXP _lidR_itc_expandcrowns(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_itc_treetops(SEXP, SEXP);
extern SEXP _lidR_MorphologicalOpening(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_point_in_polygon(SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_points_in_polygon(SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_points_in_polygons(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_tsearch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_knn(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_knnidw(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _lidR_update_list_by_ref(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_lidR_algo_li2012",          (DL_FUNC) &_lidR_algo_li2012,          7},
    {"_lidR_fast_countbelow",      (DL_FUNC) &_lidR_fast_countbelow,      2},
    {"_lidR_fast_countequal",      (DL_FUNC) &_lidR_fast_countequal,      2},
    {"_lidR_fast_countover",       (DL_FUNC) &_lidR_fast_countover,       2},
    {"_lidR_fast_table",           (DL_FUNC) &_lidR_fast_table,           2},
    {"_lidR_fast_extract",         (DL_FUNC) &_lidR_fast_extract,         6},
    {"_lidR_tinfo",                (DL_FUNC) &_lidR_tinfo,                2},
    {"_lidR_itc_expandcrowns",     (DL_FUNC) &_lidR_itc_expandcrowns,     5},
    {"_lidR_itc_treetops",         (DL_FUNC) &_lidR_itc_treetops,         2},
    {"_lidR_MorphologicalOpening", (DL_FUNC) &_lidR_MorphologicalOpening, 5},
    {"_lidR_point_in_polygon",     (DL_FUNC) &_lidR_point_in_polygon,     4},
    {"_lidR_points_in_polygon",    (DL_FUNC) &_lidR_points_in_polygon,    4},
    {"_lidR_points_in_polygons",   (DL_FUNC) &_lidR_points_in_polygons,   5},
    {"_lidR_tsearch",              (DL_FUNC) &_lidR_tsearch,              6},
    {"_lidR_knn",                  (DL_FUNC) &_lidR_knn,                  5},
    {"_lidR_knnidw",               (DL_FUNC) &_lidR_knnidw,               6},
    {"_lidR_update_list_by_ref",   (DL_FUNC) &_lidR_update_list_by_ref,   3},
    {NULL, NULL, 0}
};

void R_init_lidR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
