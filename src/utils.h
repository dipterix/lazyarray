
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r, cpp)]]

template <typename T, typename I>
bool contains(T vec, SEXP el);

SEXP getListElement(SEXP list, const char *str); 
SEXP getListElement2(SEXP list, const char *str, const SEXP ifNull); 

// [[Rcpp::export]]
SEXP dropDimension(SEXP x);

// [[Rcpp::export]]
int64_t prod2(SEXP x, bool na_rm = false);

// [[Rcpp::export]]
SEXP parseDots(Rcpp::Environment& env, bool eval);

// [[Rcpp::export]]
bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError = true);

// [[Rcpp::export]]
SEXPTYPE getSexpType(SEXP x);

// [[Rcpp::export]]
SEXP tik();

// [[Rcpp::export]]
SEXP tok(std::string msg, bool stop = false);

std::string as_dirpath(std::string x);

SEXP captureException( const std::exception& e );
SEXP makeException( std::string msg );

#endif // DIP_LAZYARRAY_UTILS_H
