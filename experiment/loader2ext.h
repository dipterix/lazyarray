// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H
#define LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H

#include "Rcpp.h"

SEXP subsetFST_double(const std::string& rootPath, const Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_integer(const std::string& rootPath, const Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_character(const std::string& rootPath, const Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

SEXP subsetFST_complex(const std::string& rootPath, const Rcpp::NumericVector& dim, const Rcpp::List& subparsed);

#endif // LAZYARRAY_LOADER2_MULTIPART_DOUBLE_H
