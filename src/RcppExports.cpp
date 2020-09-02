// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_create_lazyarray
SEXP cpp_create_lazyarray(SEXP& x, IntegerVector& dim, SEXP fileName, SEXP compression, SEXP uniformEncoding);
RcppExport SEXP _lazyarray_cpp_create_lazyarray(SEXP xSEXP, SEXP dimSEXP, SEXP fileNameSEXP, SEXP compressionSEXP, SEXP uniformEncodingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< SEXP >::type fileName(fileNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type compression(compressionSEXP);
    Rcpp::traits::input_parameter< SEXP >::type uniformEncoding(uniformEncodingSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_create_lazyarray(x, dim, fileName, compression, uniformEncoding));
    return rcpp_result_gen;
END_RCPP
}
// cpp_load_lazyarray
SEXP cpp_load_lazyarray(StringVector& files, List& partition_locations, IntegerVector& partition_dim, R_xlen_t ndim, SEXP value_type);
RcppExport SEXP _lazyarray_cpp_load_lazyarray(SEXP filesSEXP, SEXP partition_locationsSEXP, SEXP partition_dimSEXP, SEXP ndimSEXP, SEXP value_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector& >::type files(filesSEXP);
    Rcpp::traits::input_parameter< List& >::type partition_locations(partition_locationsSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type partition_dim(partition_dimSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type ndim(ndimSEXP);
    Rcpp::traits::input_parameter< SEXP >::type value_type(value_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_load_lazyarray(files, partition_locations, partition_dim, ndim, value_type));
    return rcpp_result_gen;
END_RCPP
}
// cpp_fst_retrieve
SEXP cpp_fst_retrieve(Rcpp::String fileName, SEXP colSel, SEXP start, SEXP end);
RcppExport SEXP _lazyarray_cpp_fst_retrieve(SEXP fileNameSEXP, SEXP colSelSEXP, SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type fileName(fileNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type colSel(colSelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type start(startSEXP);
    Rcpp::traits::input_parameter< SEXP >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_fst_retrieve(fileName, colSel, start, end));
    return rcpp_result_gen;
END_RCPP
}
// cpp_fst_meta
SEXP cpp_fst_meta(Rcpp::String fileName);
RcppExport SEXP _lazyarray_cpp_fst_meta(SEXP fileNameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type fileName(fileNameSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_fst_meta(fileName));
    return rcpp_result_gen;
END_RCPP
}
// test_fstcore_write
SEXP test_fstcore_write(String filename);
RcppExport SEXP _lazyarray_test_fstcore_write(SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(test_fstcore_write(filename));
    return rcpp_result_gen;
END_RCPP
}
// cpp_fst_range
SEXP cpp_fst_range(Rcpp::String fileName, String colSel, SEXP start, SEXP end, int method, bool allow_na, Rcpp::Nullable<Rcpp::Function> custom_func, Rcpp::Nullable<IntegerVector> reshape);
RcppExport SEXP _lazyarray_cpp_fst_range(SEXP fileNameSEXP, SEXP colSelSEXP, SEXP startSEXP, SEXP endSEXP, SEXP methodSEXP, SEXP allow_naSEXP, SEXP custom_funcSEXP, SEXP reshapeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type fileName(fileNameSEXP);
    Rcpp::traits::input_parameter< String >::type colSel(colSelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type start(startSEXP);
    Rcpp::traits::input_parameter< SEXP >::type end(endSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    Rcpp::traits::input_parameter< bool >::type allow_na(allow_naSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::Function> >::type custom_func(custom_funcSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<IntegerVector> >::type reshape(reshapeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_fst_range(fileName, colSel, start, end, method, allow_na, custom_func, reshape));
    return rcpp_result_gen;
END_RCPP
}
// loc2idx
IntegerVector loc2idx(List& locations, IntegerVector& parent_dim);
RcppExport SEXP _lazyarray_loc2idx(SEXP locationsSEXP, SEXP parent_dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type locations(locationsSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type parent_dim(parent_dimSEXP);
    rcpp_result_gen = Rcpp::wrap(loc2idx(locations, parent_dim));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lazyarray_cpp_create_lazyarray", (DL_FUNC) &_lazyarray_cpp_create_lazyarray, 5},
    {"_lazyarray_cpp_load_lazyarray", (DL_FUNC) &_lazyarray_cpp_load_lazyarray, 5},
    {"_lazyarray_cpp_fst_retrieve", (DL_FUNC) &_lazyarray_cpp_fst_retrieve, 4},
    {"_lazyarray_cpp_fst_meta", (DL_FUNC) &_lazyarray_cpp_fst_meta, 1},
    {"_lazyarray_test_fstcore_write", (DL_FUNC) &_lazyarray_test_fstcore_write, 1},
    {"_lazyarray_cpp_fst_range", (DL_FUNC) &_lazyarray_cpp_fst_range, 8},
    {"_lazyarray_loc2idx", (DL_FUNC) &_lazyarray_loc2idx, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lazyarray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
