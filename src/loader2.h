// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r,cpp)]]

// [[Rcpp::export]]
SEXP subsetFSTBare(const std::string& rootPath, const Rcpp::List& subparsed, const Rcpp::NumericVector& dim, const SEXPTYPE& dtype);

// [[Rcpp::export]]
SEXP subsetFST(const std::string& rootPath, SEXP listOrEnv, const Rcpp::NumericVector& dim, 
                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false);


#endif // LAZYARRAY_LOADER2_H
