// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "common.h"
#include "indexConvert.h"


// [[Rcpp::export]]
SEXP lazySubset(Rcpp::StringVector& files, Rcpp::Environment& env, Rcpp::NumericVector& dim, SEXP samp);


#endif // LAZYARRAY_LOADER2_H
