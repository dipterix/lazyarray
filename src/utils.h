
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

// [[Rcpp::plugins("cpp11")]]

Rcpp::DataFrame cpp_array_to_list(SEXP x, IntegerVector cutoff);

// SEXP cpp_create_lazyarray(SEXP x, IntegerVector dim, SEXP fileName, SEXP compression, SEXP uniformEncoding);

IntegerVector cpp_index_to_index(IntegerVector& idx, List& locations, IntegerVector& parent_dim);


#endif // DIP_LAZYARRAY_UTILS_H
