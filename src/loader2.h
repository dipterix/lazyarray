// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_LOADER2_H
#define LAZYARRAY_LOADER2_H

#include "Rcpp.h"
#include "classIndexSchedule.h"

// [[Rcpp::interfaces(r,cpp)]]

SEXP subsetFSTBare(const std::string& rootPath, const ParsedIndex* parsed, const ScheduledIndex& schedule,
                   const std::vector<int64_t>& dim, const SEXPTYPE& dtype);

// [[Rcpp::export]]
SEXP subsetFST(const std::string& rootPath, SEXP listOrEnv, const std::vector<int64_t>& dim, 
                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false);


// [[Rcpp::export]]
SEXP scheduleFST(SEXP listOrEnv, const std::vector<int64_t>& dim, bool forceSchedule = false, int64_t hint = -1);

// [[Rcpp::export]]
SEXP executeScheduleFST(const std::string& rootPath, SEXPTYPE dtype, SEXP reshape, bool drop, int64_t partition);

// [[Rcpp::export]]
SEXP scheduleExistsFST();

// [[Rcpp::export]]
SEXP freeScheduleFST();

#endif // LAZYARRAY_LOADER2_H
