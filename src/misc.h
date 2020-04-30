// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_MISC_H
#define LAZYARRAY_MISC_H

#include "common.h"
#include "utils.h"

// A misc file that contains all the duplicated code

SEXP cpp_load_lazyarray_base_real(
    Rcpp::String& fileName, IntegerVector& target_dim,
    IntegerVector& first_indices, IntegerVector& second_indices);


#endif // LAZYARRAY_MISC_H
