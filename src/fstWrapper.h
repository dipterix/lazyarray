// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_FST_H
#define LAZYARRAY_FST_H

#include <Rcpp.h>

// [[Rcpp::export]]
SEXP fstMeta(SEXP fileName);

// [[Rcpp::export]]
SEXP fstRetrieve(SEXP fileName, SEXP colSel, SEXP start, SEXP end);

/*
 * Validate fst header information
 * @param file fst file path 
 * @param expect_nrows expected number of rows
 * @param cnames column names that must contain
 */
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
bool checkFstMeta(const Rcpp::String file, const int64_t expect_nrows, Rcpp::StringVector cnames);

#endif // LAZYARRAY_FST_H
