// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r,cpp)]]

// [[Rcpp::export]]
SEXP subsetFSTBare(Rcpp::StringVector& files, const Rcpp::List& subparsed, const Rcpp::NumericVector& dim, const SEXPTYPE& dtype);

// [[Rcpp::export]]
SEXP subsetFST(Rcpp::StringVector& files, SEXP listOrEnv, const Rcpp::NumericVector& dim, 
                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false);


#endif // LAZYARRAY_LOADER2_H
