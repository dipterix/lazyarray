
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

// [[Rcpp::plugins("cpp11")]]

Rcpp::List cpp_array_to_list(SEXP &x, IntegerVector &cutoff);

// SEXP cpp_create_lazyarray(SEXP x, IntegerVector dim, SEXP fileName, SEXP compression, SEXP uniformEncoding);

// [[Rcpp::export]]
IntegerVector cpp_index_to_index(IntegerVector& idx, List& locations, IntegerVector& parent_dim);

template <typename T, typename I>
bool contains(T vec, SEXP el);


#endif // DIP_LAZYARRAY_UTILS_H
