// [[Rcpp::plugins("cpp11")]]

#include "common.h"
#include "util.h"
#include "misc.h"

// [[Rcpp::export]]
SEXP cpp_create_lazyarray(SEXP x, IntegerVector dim, SEXP fileName,
                          SEXP compression, SEXP uniformEncoding){

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
  Rcpp::DataFrame table = cpp_array_to_list(x, cutoff);
  // Rcpp::print(table);
  // Rcpp::print(fileName);
  fstcore::fststore(fileName, table, compression, uniformEncoding);
  return R_NilValue;
}


// [[Rcpp::export]]
SEXP cpp_load_lazyarray(Rcpp::String& fileName, List& locations, IntegerVector& dim, int type){
  R_xlen_t ndim = dim.size();
  if( ndim != locations.size() || ndim < 2 ){
    stop("Dimension not match for cpp_load_lazyarray");
  }
  IntegerVector first_dim = no_init(ndim - 1);
  R_xlen_t first_len = 1;
  R_xlen_t max_first_len = 1;
  List first_loc(ndim - 1);
  IntegerVector target_dim = no_init(ndim);
  IntegerVector last_indices = IntegerVector(locations[ndim - 1]);

  for(R_xlen_t ii = 0; ii < ndim; ii++){
    target_dim[ii] = (as<IntegerVector>(locations[ii])).size();
    if( ii < ndim - 1 ){
      first_loc[ii] = locations[ii];
      first_dim[ii] = dim[ii];
      first_len *= target_dim[ii];
      max_first_len *= first_dim[ii];
    }
  }

  // What if re_len is 0?
  if( first_len == 0 || dim[ndim - 1] == 0 ){
    // return numeric(0)?
  }

  IntegerVector tmp = seq_len(first_len);
  IntegerVector first_indices = cpp_index_to_index(tmp, first_loc, first_dim);

  // print(first_indices);
  // stop("asdadsa");

  // print(first_indices);
  SEXP re = cpp_load_lazyarray_base_real(fileName, target_dim, first_indices, last_indices);

  return re;
}


/*** R
x = 1:10
# cpp_array_to_list_integer(x, c(2L, 4L, 100L))

cpp_create_lazyarray()

a = lazyarray:::cpp_array_to_list(x, c(0L, 2L, 4L, 10L, length(x)+1), 9L)
a
*/
