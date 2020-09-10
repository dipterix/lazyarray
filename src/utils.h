
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "Rcpp.h"
#include "common.h"

// [[Rcpp::interfaces(r, cpp)]]

template <typename T, typename I>
bool contains(T vec, SEXP el);

SEXP getListElement(SEXP list, const char *str); 

// [[Rcpp::export]]
SEXP dropDimension(SEXP x);

// [[Rcpp::export]]
int64_t prod2(SEXP x, bool na_rm = false);

// [[Rcpp::export]]
SEXP parseDots(Rcpp::Environment& env, bool eval);

// [[Rcpp::export]]
bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError = true);


// std::vector<int64_t> numericVector2Int64tVec(const Rcpp::NumericVector& x);
// 
// int64_t doubleInt64t(const double& x);
// 
// double int64t2double(const int64_t& x);
// 
// Rcpp::NumericVector int64tVec2NumericVector(const std::vector<int64_t>& x);
// 
// std::vector<int64_t> sexp2Int64tVec(SEXP x);
// 
// SEXP int64t2Sexp(const std::vector<int64_t>& x);


#endif // DIP_LAZYARRAY_UTILS_H
