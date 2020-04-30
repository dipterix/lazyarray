
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::interfaces(cpp)]]

// [[Rcpp::export]]
Rcpp::DataFrame cpp_array_to_list(SEXP x, IntegerVector cutoff, int type = -1);

// [[Rcpp::export]]
SEXP cpp_create_lazyarray(SEXP x, IntegerVector dim, SEXP fileName, SEXP compression, SEXP uniformEncoding);

// [[Rcpp::export]]
IntegerVector cpp_index_to_index(IntegerVector& idx, List& locations, IntegerVector& parent_dim);

template <class T, typename I>
Rcpp::DataFrame cpp_array_to_list_template(T x, IntegerVector cutoff);

#endif // DIP_LAZYARRAY_UTILS_H
