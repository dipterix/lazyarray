// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER1_H
#define LAZYARRAY_LOADER1_H

#include <Rcpp.h>

// [[Rcpp::export]]
SEXP lazyLoadOld(Rcpp::StringVector& files, Rcpp::List& partition_locations, 
                        Rcpp::IntegerVector& partition_dim, R_xlen_t ndim, SEXP value_type);


#endif // LAZYARRAY_LOADER1_H
