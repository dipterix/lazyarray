// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_INDEX_H
#define LAZYARRAY_INDEX_H

#include "common.h"

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
Rcpp::IntegerVector loc2idx(Rcpp::List& locations, Rcpp::IntegerVector& parent_dim);

// [[Rcpp::export]]
Rcpp::NumericVector loc2idx2(Rcpp::List& locations, Rcpp::NumericVector& parent_dim);

// [[Rcpp::export]]
std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim);

// [[Rcpp::export]]
SEXP subsetIdx(Rcpp::Environment expr_env, Rcpp::NumericVector dim, bool pos_subscript = false);

// [[Rcpp::export]]
SEXP subsetIdx2(const Rcpp::List sliceIdx, Rcpp::NumericVector dim, bool pos_subscript = false);

// [[Rcpp::export]]
List scheduleIndexing(SEXP locations, SEXP dimension);


// [[Rcpp::export]]
List parseAndScheduleBlocks(SEXP sliceIdx, NumericVector dim);

// [[Rcpp::export]]
SEXP reshapeOrDrop(SEXP x, SEXP reshape = R_NilValue, bool drop = false);
  
#endif // LAZYARRAY_INDEX_H
