// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_SAVER2_H
#define LAZYARRAY_SAVER2_H

#include "Rcpp.h"
#include "utils.h"

// [[Rcpp::interfaces(r, cpp)]]

template <SEXPTYPE RTYPE>
SEXP writeFstPartition(const Rcpp::Vector<RTYPE>& values, const std::string& file, 
                       const std::vector<int64_t>& dim, const Rcpp::List& subparsed,
                       int compression, bool uniformEncoding);

// [[Rcpp::export]]
SEXP subsetAssignFST(const SEXP values, const std::string& file, SEXP listOrEnv,
                     const std::vector<int64_t>& dim, const SEXPTYPE& dtype,
                     int compression = 50, bool uniformEncoding = true);

#endif  // LAZYARRAY_SAVER2_H
