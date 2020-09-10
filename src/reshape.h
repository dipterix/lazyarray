// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_RESHAPE_H
#define LAZYARRAY_RESHAPE_H

#include "lazycommon.h"

Rcpp::List arr2df(SEXP x, int64_t nrows, int64_t ncols);

#endif // LAZYARRAY_RESHAPE_H
