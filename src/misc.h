// [[Rcpp::plugins("cpp11")]]

#ifndef LAZYARRAY_MISC_H
#define LAZYARRAY_MISC_H

#include "common.h"
#include "utils.h"

// A misc file that contains all the duplicated code

SEXP cpp_load_lazyarray_base(
    CharacterVector& files, IntegerVector& partition_dim, IntegerVector& target_dim,
    IntegerVector& first_indices, IntegerVector& second_indices, int type);


#endif // LAZYARRAY_MISC_H
