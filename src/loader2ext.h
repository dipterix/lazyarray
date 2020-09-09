// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H
#define LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H

#include "common.h"

SEXP lazySubset_double(Rcpp::StringVector& files, Rcpp::NumericVector& dim, Rcpp::List& subparsed);

SEXP lazySubset_integer(Rcpp::StringVector& files, Rcpp::NumericVector& dim, Rcpp::List& subparsed);

SEXP lazySubset_character(Rcpp::StringVector& files, Rcpp::NumericVector& dim, Rcpp::List& subparsed);

SEXP lazySubset_complex(Rcpp::StringVector& files, Rcpp::NumericVector& dim, Rcpp::List& subparsed);

#endif // LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H