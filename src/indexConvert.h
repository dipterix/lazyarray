// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_INDEX_H
#define LAZYARRAY_INDEX_H

#include "common.h"



// [[Rcpp::export]]
Rcpp::IntegerVector loc2idx(Rcpp::List& locations, Rcpp::IntegerVector& parent_dim);

// [[Rcpp::export]]
Rcpp::NumericVector loc2idx2(Rcpp::List& locations, Rcpp::NumericVector& parent_dim);

std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim);

// [[Rcpp::export]]
SEXP subsetIdx(Rcpp::Environment expr_env, Rcpp::NumericVector dim, bool pos_subscript);

#endif // LAZYARRAY_INDEX_H
