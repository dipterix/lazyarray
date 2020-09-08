
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

template <typename T, typename I>
bool contains(T vec, SEXP el);

SEXP getListElement(SEXP list, const char *str); 

// [[Rcpp::export]]
SEXP dropDimension(SEXP x);

// [[Rcpp::export]]
int64_t prod2(SEXP x, bool na_rm = false);

// [[Rcpp::export]]
SEXP parseDots(Rcpp::Environment& env, bool eval);



#endif // DIP_LAZYARRAY_UTILS_H
