// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H
#define LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H

#include "lazycommon.h"

SEXP subsetFST_double(Rcpp::StringVector& files, Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_integer(Rcpp::StringVector& files, Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_character(Rcpp::StringVector& files, Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_complex(Rcpp::StringVector& files, Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

#endif // LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H
