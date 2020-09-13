#include "lazyarray.h"
#include "utils.h"
using namespace lazyarray;

// expose c++ classes

/***************************************************************
 ** Constructors
 ***************************************************************/

RcppExport SEXP LazyArrayBase__new(SEXP dimension, SEXP dataType) {
  std::vector<int64_t> v2 = as<std::vector<int64_t>>(dimension);
  SEXPTYPE v3 = as<SEXPTYPE>(dataType);
  Rcpp::XPtr<LazyArrayBase> ptr( new LazyArrayBase(v2,v3), true );
  return ptr;
}

RcppExport SEXP FstArray__new(SEXP rootPath, SEXP dimension, SEXP dataType, SEXP compression, SEXP uniformEncoding) {

  std::string v1 = as<std::string>(rootPath);
  std::vector<int64_t> v2 = as<std::vector<int64_t>>(dimension);
  SEXPTYPE v3 = as<SEXPTYPE>(dataType);
  int v4 = as<int>(compression);
  bool v5 = as<bool>(uniformEncoding);
  Rcpp::XPtr<FstArray> ptr( new FstArray(v1,v2,v3,v4,v5), true );
  return ptr;
}

RcppExport SEXP FstMatrix__new(SEXP rootPath, SEXP dimension, SEXP transposed, SEXP dataType, SEXP compression, SEXP uniformEncoding) {
  std::string v1 = as<std::string>(rootPath);
  std::vector<int64_t> v2 = as<std::vector<int64_t>>(dimension);
  SEXPTYPE v3 = as<SEXPTYPE>(dataType);
  int v4 = as<int>(compression);
  bool v5 = as<bool>(uniformEncoding);
  bool v6 = as<bool>(transposed);
  Rcpp::XPtr<FstMatrix> ptr( new FstMatrix(v1,v2,v6,v3,v4,v5), true );
  return ptr;
}


/***************************************************************
 ** fields or members 
 ***************************************************************/

RcppExport SEXP LazyArrayBase__nparts(SEXP xp) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  return wrap(ptr->nparts());
}

RcppExport SEXP LazyArrayBase__dataType(SEXP xp) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  SEXPTYPE dtype = ptr->dataType();
  switch(dtype){
  case REALSXP: return wrap("double");
  case INTSXP: return wrap("integer");
  case CPLXSXP: return wrap("complex");
  case STRSXP: return wrap("character");
  default: return wrap("unknown");
  }
}

RcppExport SEXP LazyArrayBase__partLen(SEXP xp) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  return wrap(ptr->partLen());
}

RcppExport SEXP LazyArrayBase__getDim(SEXP xp) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  return wrap(ptr->getDim());
}

RcppExport SEXP LazyArrayBase__readOnly(SEXP xp, SEXP setReadOnly = R_NilValue) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  
  if(!Rf_isNull(setReadOnly)){
    // set 
    bool v = as<bool>(setReadOnly);
    ptr->setReadOnly(v);
  }
  return wrap(ptr->getReadOnly());
}

RcppExport SEXP LazyArrayBase__validate(SEXP xp, SEXP stopIfError = R_NilValue) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  if(!Rf_isNull(stopIfError)){
    return wrap(ptr->validate());
  } else {
    bool v = as<bool>(stopIfError);
    return wrap(ptr->validate(v));
  }
}

RcppExport SEXP LazyArrayBase__subset(SEXP xp, SEXP listOrEnv, SEXP reshape = R_NilValue, SEXP drop = R_NilValue) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  bool isDrop = false;
  if(!Rf_isNull(drop)){
    isDrop = as<bool>(drop);
  }
  return ptr->subset(listOrEnv, reshape, isDrop);
}

RcppExport SEXP LazyArrayBase__subsetAssign(SEXP xp, SEXP values, SEXP listOrEnv) {
  Rcpp::XPtr<LazyArrayBase> ptr(xp);
  return ptr->subsetAssign(values, listOrEnv);
}


// RCPP_MODULE(LazyArrayModules) {
// 
//   .method("scheduleBlocks", &la_scheduleBlocks)
//   .method("subset", &FstArray::subset)
//   .method("subsetAssign", &FstArray::subsetAssign)
//   ;
//   
//   Rcpp::class_< FstMatrix >( "FstMatrix" )
//     .constructor<StringVector, std::vector<int64_t>, bool, SEXPTYPE, int, bool>()
//   // field-related
//   .property("nparts", &la_nparts, &set_nothing )
//   .property("data_type", &la_dataType, &set_nothing )
//   .property("per_partLen", &la_partLen, &set_nothing )
//   .property("dim", &la_getDim, &set_nothing)
//   .property("read_only", &la_getReadOnly, &la_setReadOnly)
//   
//   // member-related
//   .method("validate", &la_validate)
//   .method("scheduleBlocks", &la_scheduleBlocks)
//   .method("subset", &FstMatrix::subset)
//   .method("subsetAssign", &FstMatrix::subsetAssign)
//   ;
// }

/*** R

setClass("FstArray", slots = c(pointer = "externalptr", method_list = "list"))


make_method_fstarray <- function(x){
  list(
    "nparts" = function(){ .Call("LazyArrayBase__nparts", x@pointer) }
  )
}

setMethod(
  "initialize", "FstArray",
  function(.Object, rootPath, dimension, dataType = "double", compression = 50, uniformEncoding = TRUE) {
    if(is.numeric(dataType)){
      dataType <- as.integer(dataType)
    } else if(is.character(dataType)){
      dataType <- list("int" = 13L, "integer" = 13L, "numeric" = 14L, "double" = 14L, "complex" = 15L, "character" = 16L, "string" = 16L)[[dataType]]
    }
    if(!isTRUE(is.integer(dataType))){
      stop("dataType must be character or integer. Choices are: integer(13), double(14), complex(15), or string(16)")
    }
    
    if(!dir.exists(rootPath)){
      dir_create(rootPath)
    }
    compression <- as.integer(compression)
    uniformEncoding <- isTRUE(uniformEncoding)
    
    .Object@pointer <- .Call("FstArray__new", rootPath, dimension, dataType, compression, uniformEncoding)
    
    # Make methods
    
    mlist <- make_method_fstarray(.Object)
    if(!is.list(.Object@method_list)){
      .Object@method_list <- mlist
    } else {
      .Object@method_list[names(mlist)] <- mlist
    }
    .Object
  }
)

setMethod(
  "$", "FstArray", function(x, name){
    return(x@method_list[[name]])
  }
)

f <- tempfile()
x <- new("FstArray", f, c(3,1))
x$nparts
x$subset(,1, drop = FALSE)


# bigm <- bigmemory::attach.resource(file.path('~/Desktop/junk/', 'bigmemory-ieeg.testfile.desc'))
# x <- lazyarray::lazyarray('~/Desktop/lazyarray_data/')
# fs <- x$get_partition_fpath()
# 
# LazyArrayBase <- lazyarray:::LazyArrayBase
# 
# mod <- new(LazyArrayBase, 'fst', fs, dim(x), 14L, list(compression = 50L, uniform_encoding = TRUE))
# # mod <- new(LazyArrayBase, 'bm', bigm@address, dim(x), 14L)
# mod$validate(TRUE)
# 
# # module <- Rcpp::Module('LazyArrayModules', PACKAGE = 'lazyarray')
# # 
# # mod <- new(module$FstArray, fs, c(prod(x$partition_dim()), 2L), 14L)
# 
# # mod$dim
# l2 <- (function(...){
#   mod$scheduleBlocks(environment())
# })(1,1,1,1)
# 
# li <- (function(...){
#   mod$scheduleBlocks(list(...))
# })(1,1)
# 
# li <- (function(i, ...){
#   mod$scheduleBlocks(environment())
# })(1,1:2, 1:3)
# 
# 
# (function(...){
#   mod$subset(environment(), NULL, FALSE)
# })(1,1)
# 
# (function(i, ...){
#   mod$subset(environment(), NULL, FALSE)
# })(1,1)
# 
# (function(...){
#   mod$subset(list(...), NULL, FALSE)
# })(1,1,1,1)




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
