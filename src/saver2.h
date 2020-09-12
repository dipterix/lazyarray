// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_SAVER2_H
#define LAZYARRAY_SAVER2_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
SEXP subsetAssignFST(const SEXP values, const Rcpp::StringVector& files, SEXP listOrEnv,
                     const Rcpp::NumericVector& dim, const SEXPTYPE& dtype,
                     int compression = 50, bool uniformEncoding = true);

#endif  // LAZYARRAY_SAVER2_H
