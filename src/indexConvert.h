// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_INDEX_H
#define LAZYARRAY_INDEX_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r, cpp)]]

// Though efficient, but no longer used as it errors when indexing >= 2^31 length
Rcpp::IntegerVector loc2idx(Rcpp::List& locations, Rcpp::IntegerVector& parent_dim);

// Same behavior as loc2idx3, but uses NumericVector, which converts int64_t back to double
// no need to do so
Rcpp::NumericVector loc2idx2(Rcpp::List& locations, Rcpp::NumericVector& parent_dim);

// [[Rcpp::export]]
std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim);

// subsetIdx and subsetIdx2 should not be used directly as parseSlices combines them all
SEXP subsetIdx(Rcpp::Environment expr_env, Rcpp::NumericVector dim, bool pos_subscript = false);

SEXP subsetIdx2(const Rcpp::List sliceIdx, const std::vector<int64_t>& dim, bool pos_subscript = false);

// should not be called directly, use parseAndScheduleBlocks instead
Rcpp::List scheduleIndexing(SEXP locations, SEXP dimension, bool forceSchedule = false);

// [[Rcpp::export]]
Rcpp::List extractSlices(SEXP listOrEnv, const R_xlen_t& ndims);

// parseSlices = subsetIdx or subsetIdx2
// WARNING: Always use pos_subscript if you want to use subset or subsetAssign functions in lazyarray
// pos_subscript=false subset is not implemented
// [[Rcpp::export]]
Rcpp::List parseSlices(SEXP listOrEnv, const std::vector<int64_t>& dim, bool pos_subscript = true);

// parseAndScheduleBlocks = parseSlices + scheduleIndexing
// [[Rcpp::export]]
Rcpp::List parseAndScheduleBlocks(SEXP sliceIdx, Rcpp::NumericVector dim, bool forceSchedule = false);

// [[Rcpp::export]]
SEXP reshapeOrDrop(SEXP x, SEXP reshape = R_NilValue, bool drop = false);
  
#endif // LAZYARRAY_INDEX_H
