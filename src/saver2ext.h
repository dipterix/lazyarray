// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_SAVER2EXT_H
#define LAZYARRAY_SAVER2EXT_H

#include "Rcpp.h"

void writeFstPartition_double(const Rcpp::NumericVector& values, const Rcpp::StringVector& files, 
                              const Rcpp::NumericVector& dim, const Rcpp::List& subparsed,
                              int compression, bool uniformEncoding);

#endif  // LAZYARRAY_SAVER2EXT_H
