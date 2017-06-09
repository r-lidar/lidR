#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP lidR_algo_li2012(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_fast_countbelow(SEXP, SEXP);
extern SEXP lidR_fast_countequal(SEXP, SEXP);
extern SEXP lidR_fast_countover(SEXP, SEXP);
extern SEXP lidR_fast_table(SEXP, SEXP);
extern SEXP lidR_tinfo(SEXP, SEXP);
extern SEXP lidR_itc_expandcrowns(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_itc_treetops(SEXP, SEXP);
extern SEXP lidR_MorphologicalOpening(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_point_in_polygon(SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_points_in_polygon(SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_points_in_polygons(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lidR_tsearch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"lidR_algo_li2012",          (DL_FUNC) &lidR_algo_li2012,          7},
    {"lidR_fast_countbelow",      (DL_FUNC) &lidR_fast_countbelow,      2},
    {"lidR_fast_countequal",      (DL_FUNC) &lidR_fast_countequal,      2},
    {"lidR_fast_countover",       (DL_FUNC) &lidR_fast_countover,       2},
    {"lidR_fast_table",           (DL_FUNC) &lidR_fast_table,           2},
    {"lidR_tinfo",                (DL_FUNC) &lidR_tinfo,                2},
    {"lidR_itc_expandcrowns",     (DL_FUNC) &lidR_itc_expandcrowns,     5},
    {"lidR_itc_treetops",         (DL_FUNC) &lidR_itc_treetops,         2},
    {"lidR_MorphologicalOpening", (DL_FUNC) &lidR_MorphologicalOpening, 5},
    {"lidR_point_in_polygon",     (DL_FUNC) &lidR_point_in_polygon,     4},
    {"lidR_points_in_polygon",    (DL_FUNC) &lidR_points_in_polygon,    4},
    {"lidR_points_in_polygons",   (DL_FUNC) &lidR_points_in_polygons,   5},
    {"lidR_tsearch",              (DL_FUNC) &lidR_tsearch,              6},
    {NULL, NULL, 0}
};

void R_init_lidR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
