// [[Rcpp::plugins("cpp11")]]

#include "common.h"
#include "utils.h"
#include "misc.h"

/**
 * Write array to fst files
 * @param x array or vector
 * @param dim array dimension
 * @param fileName,compression,uniformEncoding passed to fstcore::fststore
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
  int64_t total_length = std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<int64_t>());
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
  fstcore::fststore(fileName, table, compression, uniformEncoding);
  return R_NilValue;
}


/**
 * For example array of dimension c(10,100,20)
 * @param files files to load from
 * @param partition_locations
 * @param partition_dim partition dimension, e.g. c(10,100,1), or c(10,100), depending on how partition is stored
 * @param ndim total number of dimensions of array, in this example it's 3
 * @param value_type value type of numbers stored
 */
// [[Rcpp::export]]
SEXP cpp_load_lazyarray(StringVector& files, List& partition_locations, 
                        IntegerVector& partition_dim, R_xlen_t ndim,  SEXP value_type){
  // files are the total files
  // locations is to each partition
  // dim is total dim
  
  Rcpp::Timer _rcpp_timer;
  _rcpp_timer.step("start cpp_load_lazyarray");
  
  // Get partition dimension size
  R_xlen_t part_dim = partition_dim.size();
  if( part_dim != partition_locations.size() || part_dim < 2 ){
    stop("Dimension not match for cpp_load_lazyarray");
  }
  
  // Obtain the dimension to be returned
  IntegerVector target_dim = sapply(partition_locations, Rf_length);
  
  // If dim for each partition shares the same length as array dim,
  if(part_dim == ndim){
    target_dim[part_dim - 1] *= files.size();
  } else {
    // multipart, mode: 2: tensor mode = part mode + 1
    // this mode is experimental
    
    if( ndim - part_dim != 1 ){
      stop("Total dimension must be partition size+1 in multipart mode~2");
    }
    
    target_dim.push_back( files.size() );
  }
  
  // int64_t target_length = std::accumulate(target_dim.begin(), target_dim.end(), 1, std::multiplies<int64_t>());
  
  // if( target_length == 0 )
  
  /*
   * split partition_locations into two parts
   * the last of partition dim is column of fst -- column_indices
   * the other dimensions need to be converted to rows of fst -- first_dim & first_loc
   */
  
  // first_dim: partition_dim[-length(partition_dim)], e.g. c(10,100)
  IntegerVector first_dim = IntegerVector(partition_dim.begin(), partition_dim.end() - 1);
  
  // column_indices = partition_dim[length(partition_dim)]
  IntegerVector column_indices = IntegerVector(partition_locations[part_dim - 1]);
  
  // Partition location indexes to be mapped to fst rows
  List first_loc(part_dim - 1);
  for(R_xlen_t ii = 0; ii < part_dim - 1; ii++){
    first_loc[ii] = partition_locations[ii];
  }
  
  _rcpp_timer.step("calculated target_dim");
  
  
  // TODO: check whether first_len is needed
  int64_t first_len = std::accumulate(target_dim.begin(), target_dim.begin() + part_dim - 1, 1, std::multiplies<int64_t>());
  // What if re_len is 0?
  if( first_len == 0 || column_indices.size() == 0 || files.size() == 0 ){
    // return numeric(0)?
    return R_NilValue;
  }

  NumericVector row_indices = loc2idx(first_loc, first_dim);
  
  _rcpp_timer.step("calculated loc2idx");

  // print(partition_dim);
  SEXP re = cpp_load_lazyarray_base(files, partition_dim, target_dim, 
                                    row_indices, column_indices, TYPEOF(value_type));
  
  _rcpp_timer.step("calculated cpp_load_lazyarray_base");
  
  // if( LAZYARRAY_DEBUG ){
  //   
  //   NumericVector _res(_rcpp_timer);
  //   _res = _res / 1000.0;
  //   Rcpp::print(_res);
  // }
  return re;
}

// [[Rcpp::export]]
SEXP cpp_fst_retrieve(Rcpp::String fileName, SEXP colSel, SEXP start, SEXP end){
  return fstcore::fstretrieve(fileName, colSel, start, end);
}

// [[Rcpp::export]]
SEXP cpp_fst_meta(Rcpp::String fileName){
  return fstcore::fstmetadata(fileName);
}

// [[Rcpp::export]]
SEXP test_fstcore_write(String filename){
  DataFrame data = DataFrame::create(_["V1"] = 1);
  return fstcore::fststore(filename, data, wrap(100), wrap(true));
}

// [[Rcpp::export]]
SEXP cpp_fst_range(Rcpp::String fileName, String colSel, SEXP start, SEXP end, int method, 
                   bool allow_na, Rcpp::Nullable<Rcpp::Function> custom_func = R_NilValue,
                   Rcpp::Nullable<IntegerVector> reshape = R_NilValue){
  // method:
  // 1: min
  // 2: max
  // 3: range
  
  List tmp = fstcore::fstretrieve(fileName, wrap(colSel), start, end);
  
  tmp = tmp["resTable"];
  NumericVector data = wrap(tmp[colSel]);
  const R_xlen_t s = data.size();
  
  SEXP re;
  
  if( method > 0 && method <= 4 ){
    if( !allow_na ){
      for( double* ptr = data.begin(); ptr!= data.end(); ptr++){
        if( R_IsNA( *ptr ) ){
          re = wrap(NA_REAL);
          Rf_setAttrib(re, wrap("chunk_length"), wrap(s));
          return re;
        }
      }
    }
  } else if (reshape != R_NilValue) {
    data.attr("dim") = as<IntegerVector>(reshape);
  }
  
  
  double m = NA_REAL;
  
  switch(method){
  case 1:
    m = *std::min_element(data.begin(), data.end());
    re = wrap(m);
    break;
  case 2:
    m = *std::max_element(data.begin(), data.end());
    re = wrap(m);
    break;
  case 3:
    m = std::accumulate( data.begin(), data.end(), 0.0 );
    re = wrap(m);
    break;
  case 4:
    m = std::inner_product( data.begin(), data.end(), data.begin(), 0.0 );
    re = wrap(m);
    break;
  default:
    if( custom_func.isNotNull() ){
      re = Rcpp::as<Rcpp::Function>(custom_func)( data );
    } else {
      re = wrap(m);
    }
    
  }
  
  if( re != R_NilValue ){
    // re.attr("chunk_length") = s;
    Rf_setAttrib(re, wrap("chunk_length"), wrap(s));
  }
  
  
  
  return re;
}



/*** R
path <- tempfile()
a = data.frame(V1=c(1:10 + rnorm(10), rep(NA,2)))
fst::write_fst(a, path)
path <- normalizePath(path)
cpp_fst_range(path, 'V1', 1L, NULL, 1L, TRUE)
cpp_fst_range(path, 'V1', 1L, NULL, 2L, TRUE)
cpp_fst_range(path, 'V1', 1L, NULL, 3L, TRUE)
cpp_fst_range(path, 'V1', 1L, NULL, 4L, TRUE)
cpp_fst_range(path, 'V1', 1L, NULL, 5L, TRUE, length)
cpp_fst_range(path, 'V1', 1L, NULL, 5L, TRUE, function(x){NULL})
cpp_fst_range(path, 'V1', 1L, NULL, 5L, TRUE, function(x){dim(x)}, c(3L,4L))

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
