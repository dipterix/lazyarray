// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "lazycommon.h"

// [[Rcpp::interfaces(r,cpp)]]


// [[Rcpp::export]]
SEXP lazySubset(Rcpp::StringVector& files, SEXP listOrEnv, Rcpp::NumericVector& dim, 
                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false);


#endif // LAZYARRAY_LOADER2_H
