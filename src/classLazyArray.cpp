#include "lazyarray.h"
#include "utils.h"
using namespace lazyarray;


// Expose class as S4 class

LazyArrayBase *newLazyArray( const std::string &arrType, SEXP filesOrPtrs, std::vector<int64_t> dimension, 
                             SEXPTYPE dataType, SEXP moreArgs ) {
  if (arrType == "fst"){
    
    int compression = 50;
    bool uniformEnc = true;
    if( !Rf_isNull(moreArgs) ){
      SEXP tmp = getListElement(moreArgs, "compression");
      if( !Rf_isNull(tmp) ){
        compression = as<int>(tmp);
      }
      tmp = getListElement(moreArgs, "uniform_encoding");
      if( !Rf_isNull(tmp) ){
        uniformEnc = as<bool>(tmp);
      }
    }
    
    return new FstArray(Rcpp::as<StringVector>(filesOrPtrs), dimension, dataType, compression, uniformEnc);
  // } else if (arrType == "bm"){
  //   Rcpp::XPtr<BigMatrix> ptr(filesOrPtrs);
  //   return new BMArray(ptr, dimension, dataType);
  } else {
    Rcpp::stop("Array type not yet supported. Only `fst` and `bm` are supported.");
  }
}


RCPP_MODULE(LazyArrayModules) {

  using namespace lazyarray;
  Rcpp::class_< LazyArrayBase >( "LazyArrayBase" )
    .factory<const std::string&,SEXP,std::vector<int64_t>,SEXPTYPE>(newLazyArray)
    .method("nparts", &LazyArrayBase::nparts)
    .method<bool>("validate", &LazyArrayBase::validate)
    .method("dataType", &LazyArrayBase::dataType)
    .method("scheduleBlocks", &LazyArrayBase::scheduleBlocks)
    .method("subset", &LazyArrayBase::subset)
    .method("subsetAssign", &LazyArrayBase::subsetAssign)
  ;
}

/*** R
bigm <- bigmemory::attach.resource(file.path('~/Desktop/junk/', 'bigmemory-ieeg.testfile.desc'))
x <- lazyarray::lazyarray('~/Desktop/lazyarray_data/')
fs <- x$get_partition_fpath()

LazyArrayBase <- lazyarray:::LazyArrayBase

mod <- new(LazyArrayBase, 'fst', fs, dim(x), 14L)
mod <- new(LazyArrayBase, 'bm', bigm@address, dim(x), 14L)
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
})(1,1,1,1)




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
