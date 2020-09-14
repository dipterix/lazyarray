// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_SAVER2EXT_H
#define LAZYARRAY_SAVER2EXT_H

#include "Rcpp.h"

SEXP writeFstPartition_double(const Rcpp::NumericVector& values, const std::string& file, 
                              const Rcpp::NumericVector& dim, const Rcpp::List& subparsed,
                              int compression, bool uniformEncoding);

#endif  // LAZYARRAY_SAVER2EXT_H
