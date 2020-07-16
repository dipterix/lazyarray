// [[Rcpp::plugins("cpp11")]]

#include "common.h"
#include "utils.h"
#include "misc.h"

// [[Rcpp::export]]
SEXP cpp_create_lazyarray(SEXP& x, IntegerVector& dim, String& fileName,
                          int compression, bool uniformEncoding){

  R_xlen_t ndim = dim.size();

  if( ndim < 1 ){
    // shouldn't enter here
    Rcpp::stop("cpp_create_lazyarray needs at least one dim. It can be the length.");
  }

  int last_dim = 1;
  int first_dim = 1;
  if( ndim < 2 ){
    // this is a vector
    ndim = 2;
    first_dim = dim[0];
  } else {
    last_dim = dim[ ndim - 1 ];
    for(auto& ii : dim){
      first_dim *= ii;
    }
    first_dim = first_dim / last_dim;
  }
  // Calculate
  IntegerVector cutoff(last_dim + 1);
  cutoff[0] = 0;

  for(R_xlen_t jj = 1; jj <= last_dim; jj ++ ){
    cutoff[jj] = cutoff[jj - 1] + first_dim;
  }
  // Rcpp::print(cutoff);
  // Rcpp::List table(0);
  Rcpp::List table = cpp_array_to_list(x, cutoff);
  // return table;
  // Rcpp::print(table);
  fstcore::fststore(fileName, table, wrap(compression), wrap(uniformEncoding));
  return R_NilValue;
}


// [[Rcpp::export]]
SEXP cpp_load_lazyarray(StringVector& files, List& partition_locations, 
                        IntegerVector& partition_dim, R_xlen_t ndim,  SEXP value_type){
  // files are the total files
  // locations is to each partition
  // dim is total dim
  R_xlen_t part_dim = partition_dim.size();
  if( part_dim != partition_locations.size() || part_dim < 2 ){
    stop("Dimension not match for cpp_load_lazyarray");
  }
  
  IntegerVector first_dim = no_init(part_dim - 1);
  R_xlen_t first_len = 1;
  R_xlen_t max_first_len = 1;
  List first_loc(part_dim - 1);
  IntegerVector target_dim = no_init(ndim);
  IntegerVector last_indices = IntegerVector(partition_locations[part_dim - 1]);

  for(R_xlen_t ii = 0; ii < part_dim; ii++){
    target_dim[ii] = (as<IntegerVector>(partition_locations[ii])).size();
    if( ii < part_dim - 1 ){
      first_loc[ii] = partition_locations[ii];
      first_dim[ii] = partition_dim[ii];
      first_len *= target_dim[ii];
      max_first_len *= first_dim[ii];
    }
  }
  if(part_dim == ndim){
    target_dim[ndim - 1] *= files.size();
  } else {
    // multipart, mode: 2: tensor mode = part mode + 1
    target_dim[ndim - 1] = files.size();
  }
  

  // What if re_len is 0?
  if( first_len == 0 || last_indices.size() == 0 || files.size() == 0 ){
    // return numeric(0)?
    return R_NilValue;
  }

  IntegerVector tmp = seq_len(first_len);
  IntegerVector first_indices = cpp_index_to_index(tmp, first_loc, first_dim);

  // print(partition_dim);
  SEXP re = cpp_load_lazyarray_base(files, partition_dim, target_dim, 
                                    first_indices, last_indices, TYPEOF(value_type));
 
  return re;
}


// [[Rcpp::export]]
SEXP test_fstcore_write(String filename){
  DataFrame data = DataFrame::create(_["V1"] = 1);
  return fstcore::fststore(filename, data, wrap(100), wrap(true));
}

// [[Rcpp::export]]
SEXP cpp_fst_meta_orig(Rcpp::String fileName){
  return fstcore::fstmetadata(fileName);
}



/*** R
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
