#include "utils.h"

#include "lazyarray.h"
using namespace Rcpp;


// Expose class as S4 class

// RCPP_MODULE(LazyArrayModules) {
//   
//   using namespace lazyarray;
//   
//   Rcpp::class_<FstArray>( "FstArray" )
//   .constructor<StringVector, std::vector<int64_t>, SEXPTYPE>()
//   
//   .field_readonly("dim", &FstArray::LazyArrayBase::dimension)
//   
//   .method("nparts", &FstArray::nparts)
//   .method("validate", &FstArray::validate)
//   .method("dataType", &FstArray::dataType)
//   .method("scheduleBlocks", &FstArray::scheduleBlocks)
//   .method("subset", &FstArray::subset)
//   ;
// }

/*** R
x <- lazyarray::lazyarray('~/Desktop/lazyarray_data/')
fs <- x$get_partition_fpath(1:3)


mod <- new(FstArray, fs, c(prod(x$partition_dim()), 3L), 14L)
mod$validate(TRUE)

# module <- Rcpp::Module('LazyArrayModules', PACKAGE = 'lazyarray')
# 
# mod <- new(module$FstArray, fs, c(prod(x$partition_dim()), 2L), 14L)

mod$dim
l2 <- (function(...){
  mod$scheduleBlocks(environment())
})(1,1)

li <- (function(...){
  mod$scheduleBlocks(list(...))
})(1,1)

li <- (function(i, ...){
  mod$scheduleBlocks(environment())
})(1,1:2, 1:3)


(function(...){
  mod$subset(environment(), NULL, FALSE)
})(1,1)

(function(i, ...){
  mod$subset(environment(), NULL, FALSE)
})(1,1)

(function(...){
  mod$subset(list(...), NULL, FALSE)
})(1,1)




# subsetIdx2(list(1,-1), c(3,2), TRUE)

# const int subset_mode = subparsed["subset_mode"];
# const NumericVector target_dimension = subparsed["target_dimension"];
# const int64_t expected_length = subparsed["expected_length"];
# const LogicalVector negative_subscript = subparsed["negative_subscript"];
# List location_indices = subparsed["location_indices"];


# Rcpp::loadModule('LazyArrayModules', loadNow = TRUE, env = asNamespace('lazyarray'))



# module <- Rcpp::Module('LazyArrayModules', PACKAGE = 'lazyarray')
# 
# mod <- new(module$FstArray, "", c(1L, 2L), 1L)


*/
