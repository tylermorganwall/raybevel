// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// is_ccw_polygon
bool is_ccw_polygon(NumericMatrix vertices);
RcppExport SEXP _rayskeleton_is_ccw_polygon(SEXP verticesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type vertices(verticesSEXP);
    rcpp_result_gen = Rcpp::wrap(is_ccw_polygon(vertices));
    return rcpp_result_gen;
END_RCPP
}
// is_simple_polygon
bool is_simple_polygon(NumericMatrix vertices);
RcppExport SEXP _rayskeleton_is_simple_polygon(SEXP verticesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type vertices(verticesSEXP);
    rcpp_result_gen = Rcpp::wrap(is_simple_polygon(vertices));
    return rcpp_result_gen;
END_RCPP
}
// skeletonize_rcpp
List skeletonize_rcpp(NumericMatrix vertices, List holes, double offset);
RcppExport SEXP _rayskeleton_skeletonize_rcpp(SEXP verticesSEXP, SEXP holesSEXP, SEXP offsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type vertices(verticesSEXP);
    Rcpp::traits::input_parameter< List >::type holes(holesSEXP);
    Rcpp::traits::input_parameter< double >::type offset(offsetSEXP);
    rcpp_result_gen = Rcpp::wrap(skeletonize_rcpp(vertices, holes, offset));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rayskeleton_is_ccw_polygon", (DL_FUNC) &_rayskeleton_is_ccw_polygon, 1},
    {"_rayskeleton_is_simple_polygon", (DL_FUNC) &_rayskeleton_is_simple_polygon, 1},
    {"_rayskeleton_skeletonize_rcpp", (DL_FUNC) &_rayskeleton_skeletonize_rcpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_rayskeleton(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
