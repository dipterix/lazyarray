// [[Rcpp::plugins("cpp11")]]

#include "utils.h"
#include "reshape.h"
#include "loader1.h"
#include "loader2.h"
#include "fstWrapper.h"

#include "lazycommon.h"

using namespace Rcpp; 

/**
 * Write array to fst files
 * @param x array or vector
 * @param dim array dimension
 * @param fileName,compression,uniformEncoding passed to fstStore
 */
// [[Rcpp::export]]
SEXP cpp_create_lazyarray(SEXP& x, IntegerVector& dim, SEXP fileName,
                          SEXP compression, SEXP uniformEncoding){
  
  // Obtain the array dimension, dimension length must >= 1
  R_xlen_t ndim = dim.size();

  if( ndim < 1 ){
    // shouldn't enter here
    Rcpp::stop("cpp_create_lazyarray needs at least one dim. It can be the length.");
  }

  /*
   * Reshape dimension to two dimension: [first_dim, last_dim]
   * Case 1: dim has length 1, e.g. dim = c(10), last_dim is always 1, and first_dim is 10
   * Case 2: dim = c(10,10,2), last_dim is always the last dim, i.e. 2, first_dim is prod of the rest, i.e. 100
   */
  int64_t last_dim = 1;
  int64_t total_length = std::accumulate(dim.begin(), dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  if( ndim >= 2 ){
    // case 2
    last_dim = *(dim.end() - 1);
  }
  int64_t first_dim = total_length / last_dim;
  
  if( total_length == 0 ){
    // no need to write to disk?
    // FIXME
  }
  
  // Need to reshape x to a data.frame/list i.e. dim(x) = c(first_dim, last_dim)
  Rcpp::List table = arr2df(x, first_dim, last_dim);
  // return table;
  // Rcpp::print(table);
  fstStore(fileName, table, compression, uniformEncoding);
  return R_NilValue;
}


/**
 * @param fileName,colSel,start,end Parameter required to locate each partition for each file
 * @param custom_func Function to apply to each partition
 * @param reshape Dimension to reshape partition data to
 */
// [[Rcpp::export]]
SEXP lazyMapReduceByPartition(
    Rcpp::String fileName, CharacterVector colSel, SEXP start, SEXP end = R_NilValue, 
    Rcpp::Nullable<Rcpp::Function> custom_func = R_NilValue,
    Rcpp::Nullable<IntegerVector> reshape = R_NilValue){
  
  SEXP tmp;
  tmp = fstRetrieve(fileName, wrap(colSel), start, end);
  
  tmp = getListElement(tmp, "resTable");
  
  SEXP data;
  R_xlen_t s;
  
  // if colSel.size() == 1, non-complex data
  bool is_complex = false;
  if( colSel.size() == 1 ){
    data = getListElement(tmp, colSel[0]);
    s = Rf_xlength(data);
  } else {
    is_complex = true;
    NumericVector re = as<NumericVector>(getListElement(tmp, colSel[0]));
    NumericVector im = as<NumericVector>(getListElement(tmp, colSel[1]));
    s = Rf_xlength(re);
    
    data = PROTECT(Rf_allocVector(CPLXSXP, s));
    
    Rcomplex *ptr_data = COMPLEX(data);
    NumericVector::iterator ptr_re = re.begin();
    NumericVector::iterator ptr_im = im.begin();
    
    for(R_xlen_t ii = 0; ii < s; ii++ ){
      (*ptr_data).r = *ptr_re;
      (*ptr_data).i = *ptr_im;
      ptr_data++;
      ptr_re++;
      ptr_im++;
    }
    
  }
  
  SEXP re;
  
  if(reshape != R_NilValue){
    Rf_setAttrib(data, wrap("dim"), wrap(reshape));
  }
  
  if( custom_func.isNotNull() ){
    re = Rcpp::as<Rcpp::Function>(custom_func)( data );
  } else {
    re = R_NilValue;
  }
  
  if( re != R_NilValue ){
    // re.attr("chunk_length") = s;
    Rf_setAttrib(re, wrap("chunk_length"), wrap(s));
  }
  
  if( is_complex ){
    UNPROTECT(1);
  }
  
  return re;
}




/*** R
# devtools::load_all()
require(lazyarray)
x <- array(seq_len(prod(c(3000,7,20,8))), c(3000,7,20,8))
x <- as.lazyarray(x)

# unlink(x$get_partition_fpath(3))
# fst::write_fst(data.frame(V2 = 1:8), x$get_partition_fpath(3))

b <- function(i,...){
  subsetIdx(environment(), dim(x), TRUE)
}
invisible(b(1:5,-c(1:2,NA),c(1,NA),))

files <- x$get_partition_fpath()
a <- function(i,...){
  lazySubset(files, environment(), dim(x), 0.1)
}
# e = a(c(3,1,7,NA,2,1, 27, 16, 15,14,NA, 27))
e = a(1:5,c(1:2,NA),,)
range(e - x[][1:5,c(1:2,NA),,], na.rm = TRUE)

a(c(1:20, NA))

# array(1:27, c(3,3,3))[c(3,1,7,NA,2,1)]


# f <- tempfile()
# fst::write_fst(data.frame(V1R = 1:10, V1I = 10:1), path = f)
# tmp <- fst:::fstretrieve(normalizePath(f), c('V1R', 'V1I'), 1L, NULL)
# tmp
# 
# path <- normalizePath(f)
# lazyMapReduceByPartition(path, c('V1R', 'V1I'), 1L, NULL, sum)
# lazyMapReduceByPartition(path, c('V1R'), 1L, NULL, sum)
# 
# x <- lazyarray(tempfile(), 'complex', c(2,3,4))
# x[] <- 1:24 + (24:1)*1i
# 
# partition_map(x, function(slice, part){
#   slice
# })


# path = '~/Desktop/lazyarray_data'
# dimension <- c(287, 200, 601, 84)
# x <- lazyarray(path, storage_format = "double", dim = dimension)
# part_loc <- list(1:287L, 1:200L, 1:601L, 1L)
# a <- bench::mark({
#   cpp_load_lazyarray(x$get_partition_fpath(1), part_loc, c(287L, 200L, 601L, 1L), 4, 0.1)
# }, iterations = 1)
# a$memory
# 
# path <- tempfile()
# a = data.frame(V1=c(1:10 + rnorm(10), rep(NA,2)))
# fst::write_fst(a, path)
# path <- normalizePath(path)
# lazyMapReduceByPartition(path, 'V1', 1L, NULL, sum)
# lazyMapReduceByPartition(path, 'V1', 1L, NULL, function(x){mean(x, na.rm = TRUE)})
# lazyMapReduceByPartition(path, 'V1', 1L, NULL, length)
# lazyMapReduceByPartition(path, 'V1', 1L, NULL, function(x){NULL})
# lazyMapReduceByPartition(path, 'V1', 1L, NULL, function(x){dim(x)}, c(3L,4L))

# a = 1:3; b = 4:6+0.5
# pryr::address(a)
# c =join_vectors(a, b)
# a
# pryr::address(a)
# 
# f <- normalizePath(tempfile(), mustWork = FALSE)
# unlink(f)
# dim <- c(10,20,50);
# x <- rnorm(10000); dim(x) <- dim
# x[sample(10000, 2000)] = NA
# 
# lazyarray:::cpp_create_lazyarray(x, dim, f, 100L, TRUE);
# 
# expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")
# 
# 
# # Make sure we have invalid indices
# idx_loc <- list(
#   as.integer(sample(12) - 1),
#   as.integer(sample(22)-1),
#   as.integer(sample(52)-1)
# )
# target_dim = sapply(idx_loc, length)
# cpp_load_lazyarray(f, idx_loc, dim, length(dim), 0.1)
*/
