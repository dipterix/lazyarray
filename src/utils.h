
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

// [[Rcpp::plugins("cpp11")]]

Rcpp::List arr2df(SEXP &x, int64_t nrows, int64_t ncols);

// SEXP cpp_create_lazyarray(SEXP x, IntegerVector dim, SEXP fileName, SEXP compression, SEXP uniformEncoding);

// [[Rcpp::export]]
NumericVector loc2idx(List& locations, IntegerVector& parent_dim);

template <typename T, typename I>
bool contains(T vec, SEXP el);


#endif // DIP_LAZYARRAY_UTILS_H
