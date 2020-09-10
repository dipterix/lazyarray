#include "utils.h"

#include "lazyarray.h"
using namespace Rcpp;


// Expose class as S4 class

RCPP_MODULE(fst_LazyArray) {
  
  using namespace lazyarray;
  
  Rcpp::class_<FstLazyArray>( "FstLazyArray" )
  .constructor<StringVector, std::vector<int64_t>, SEXPTYPE>()
  
  .field_readonly("dim", &FstLazyArray::dimension)
  
  .method("nparts", &FstLazyArray::nparts)
  .method("validate", &FstLazyArray::validate)
  .method("dataType", &FstLazyArray::dataType)
  .method("subsetBare", &FstLazyArray::subsetBare)
  .method("getSubsetIdx", &FstLazyArray::getSubsetIdx)
  .method("getSubsetIdx2", &FstLazyArray::getSubsetIdx2)
  .method("subset", &FstLazyArray::subset)
  ;
}

/*** R
x <- lazyarray::lazyarray('~/Desktop/lazyarray_data/')
fs <- x$get_partition_fpath(1:3)


mod <- new(FstLazyArray, fs, c(prod(x$partition_dim()), 3L), 14L)
mod$validate(TRUE)

# module <- Rcpp::Module('fst_LazyArray', PACKAGE = 'lazyarray')
# 
# mod <- new(module$FstLazyArray, fs, c(prod(x$partition_dim()), 2L), 14L)

mod$dim
li <- mod$getSubsetIdx2(
  list(1,-1),
  TRUE
)
mod$subsetBare(
  li,
  NULL,
  FALSE
)
(function(...){
  mod$getSubsetIdx(environment(), TRUE)
})(1,1)

(function(...){
  mod$subset(environment(), NULL, FALSE)
})(1,1)






# subsetIdx2(list(1,-1), c(3,2), TRUE)

# const int subset_mode = subparsed["subset_mode"];
# const NumericVector target_dimension = subparsed["target_dimension"];
# const int64_t expected_length = subparsed["expected_length"];
# const LogicalVector negative_subscript = subparsed["negative_subscript"];
# List location_indices = subparsed["location_indices"];


# Rcpp::loadModule('fst_LazyArray', loadNow = TRUE, env = asNamespace('lazyarray'))



# module <- Rcpp::Module('fst_LazyArray', PACKAGE = 'lazyarray')
# 
# mod <- new(module$FstLazyArray, "", c(1L, 2L), 1L)


*/
