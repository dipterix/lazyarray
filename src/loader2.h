// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "lazycommon.h"

// [[Rcpp::interfaces(r,cpp)]]

// [[Rcpp::export]]
SEXP lazySubsetBare(Rcpp::StringVector& files, Rcpp::NumericVector& dim, 
                    const List& subparsed, SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false);

// [[Rcpp::export]]
SEXP lazySubset(Rcpp::StringVector& files, Rcpp::Environment& env, Rcpp::NumericVector& dim, 
                SEXP samp, SEXP reshape = R_NilValue, bool drop = false);


#endif // LAZYARRAY_LOADER2_H
