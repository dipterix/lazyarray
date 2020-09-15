#ifndef LAZYARRAY_BM_SUBSET_H
#define LAZYARRAY_BM_SUBSET_H

#include <Rcpp.h>
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <numeric>

// [[Rcpp::interfaces(cpp)]]

// [[Rcpp::export]]
SEXP subsetBM(SEXP pBigMat, SEXP listOrEnv, Rcpp::NumericVector dim, SEXPTYPE dtype,SEXP reshape = R_NilValue, bool drop = false);

#endif // LAZYARRAY_BM_SUBSET_H
