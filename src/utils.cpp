
#include "common.h"
#include "utils.h"

template <typename T, typename I>
bool contains(T vec, SEXP el){
  for(I ptr = vec.begin(); ptr != vec.end(); vec++ ){
    if(*ptr == el){
      return true;
    }
  }
  return false;
}

template <class T, typename I>
List cpp_array_to_list_template(T x, int64_t nrows, int64_t ncols){
  // length of x has been checked so assume length(x) = nrows * ncols
  List re = List::create();
  String colname;
  I ptr1_x = x.begin();
  I ptr2_x = x.begin();
  
  
  for(int64_t ii = 0; ii < ncols; ii++ ){
    ptr2_x += nrows;
    colname = "V" + std::to_string(ii + 1);
    T slice = T(ptr1_x, ptr2_x);
    re.push_back(slice, colname);
    ptr1_x = ptr2_x;
  }
  
  
  return re;
}


List cpp_array_to_list_complex(ComplexVector x, int64_t nrows, int64_t ncols){
  
  
  List re = List::create();
  String colname;
  ComplexVector::iterator ptr1_x = x.begin();
  ComplexVector::iterator ptr2_x = x.begin();
  
  for(int64_t ii = 0; ii < ncols; ii++ ){
    ptr2_x += nrows;
    const ComplexVector slice = ComplexVector(ptr1_x, ptr2_x);
    
    colname = "V" + std::to_string(ii + 1) + "R";
    re.push_back(Armor<NumericVector>(Rcpp::Re(slice)), colname);
    
    colname = "V" + std::to_string(ii + 1) + "I";
    re.push_back(Armor<NumericVector>(Rcpp::Im(slice)), colname);
    
    ptr1_x = ptr2_x;
  }
  
  return re;
}


/**
 * Conver vector x to data.frame with dimension c(first_dim, last_dim)
 * 
 * 2020-09-02: 
 * 1. renamed from cpp_array_to_list to arr2df
 * 2. changed argument, explicitly add nrows and ncols to be memory efficient
 * 3. Added length check
 */
Rcpp::List arr2df(SEXP &x, int64_t nrows, int64_t ncols){
  // User explicitly tells which storage type of x should be
  // 9	CHARSXP	internal character strings
  // 10	LGLSXP	logical vectors
  // 13	INTSXP	integer vectors
  // 14	REALSXP	numeric vectors
  // 15	CPLXSXP	complex vectors
  // 16	STRSXP	character vectors
  // 24	RAWSXP	raw vector
  // 
  
  
  // check length of x
  if( nrows * ncols - Rf_length(x) != 0 ){
    stop("Cannot reshape array to data.frame, dimension not match");
  }
  
  Rcpp::List re; 
  switch (TYPEOF(x)) {
  case STRSXP:
  case CHARSXP:
    re = cpp_array_to_list_template<CharacterVector, CharacterVector::iterator>(x, nrows, ncols);
    break;
  case LGLSXP:
    re = cpp_array_to_list_template<LogicalVector, LogicalVector::iterator>(x, nrows, ncols);
    break;
  case INTSXP:
    re = cpp_array_to_list_template<IntegerVector, IntegerVector::iterator>(x, nrows, ncols);
    break;
  case REALSXP:
    re = cpp_array_to_list_template<NumericVector, NumericVector::iterator>(x, nrows, ncols);
    break;
  case CPLXSXP:
    re = cpp_array_to_list_complex(x, nrows, ncols);
    break;
  default:
    Rcpp::stop("Unsupported data type, only logical, numeric, complex, character types are supported.");
  }
  return re;
}

/**
 * Map location index to integer index at parent_dim
 * For example, suppose dim(a) is c(4,4), location [1,2] is at index 5 [(2-1)*4+1]
 * This means a[1,2] = a[5]
 * loc2idx converts list(1,2) to 5
 * 
 * If location is invalid, then return NA
 * 
 * 2020-09-01: This function had very poor performance indexing c(300L, 200L, 600L, 1L)
 * array requires 1.5+ seconds while R implementation only requires 500~700ms
 * 
 * 2020-09-02: I rewrite this function, 
 * 1. got rid of code and improved readability
 * 2. changed return type from IntegerVector to NumericVector (support int64_t indexing)
 * 2.1 changed back to IntegerVector for two reasons: 
 *     1. memory overhead 
 *     2. loc2idx is running on each partition, it's hard for a partition to 
 *        have very large size. 
 * 3. Removed redundant argument that can be calculated via `locations`
 * The new function only allocate (almost) minimal size and runs faster to index 
 * 36 million indexes (~400ms)
 * 
 * Changed name from cpp_index_to_index to loc2idx
 */
IntegerVector loc2idx(List& locations, IntegerVector& parent_dim){
  
  // Check whether parent_dim matches with location index size - validation
  R_xlen_t ndim = parent_dim.size();
  
  if( ndim != locations.size() ){
    stop("Dimension input wrong for `loc2idx`");
  }
  
  // Get sub-dimension for location indexes (the dimension of returned value)
  IntegerVector sub_dim = sapply(locations, Rf_length);
  
  // Total length to return
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), 1, std::multiplies<int64_t>());
  
  // Generate integer vector to be returned and assign dimension
  IntegerVector re(sub_size, 1);
  re.attr("dim") = sub_dim;
  // re.attr("class") = "integer64";
  
  if( sub_size == 0 ){
    return re;
  }
  
  // Inflate indexes and add them
  R_xlen_t ii = 0, jj = 0;
  R_xlen_t inflate = 1;
  R_xlen_t tmp = 0;
  R_xlen_t neach = 1;
  
  for(ii = 0; ii < ndim; ii++ ){
    
    tmp = parent_dim[ii];
    
    // Ger slice index
    IntegerVector location_ii = IntegerVector( locations[ii] );
    IntegerVector current_location = IntegerVector(location_ii.begin(), location_ii.end());
    
    // Assign invalid indexes to NA
    // The first needs to be is_na otherwise error will be raised: "can't subset using a logical vector with NAs"
    // current_location[ is_na(current_location) | current_location < 1 | current_location > tmp ] = NA_INTEGER;
    
    // write to re,
    // re += rep(current_location, nrepeat, neach)
    // if( neach > 1 ){
    //   current_location = Rcpp::rep_each(current_location, neach);
    // }
    // re += Rcpp::rep_len(current_location, sub_size);
    IntegerVector::iterator ptr_current_location = current_location.begin();
    for( IntegerVector::iterator ptr_re = re.begin(); ptr_re != re.end(); ){
      for(jj = 0; jj < neach; jj++){
        // if re[...] is not NA
        if(*ptr_re != NA_INTEGER && 
           
           // current_location is not NA
           *ptr_current_location != NA_INTEGER &&
           
           // current_location is valid, i.e. current_location >= 1 & current_location <= tmp
           *ptr_current_location >= 1 &&
           
           *ptr_current_location <= tmp
           
        ){
          
          // index[[...]] += (locations[[ii]] - 1) * inflate
          *ptr_re += ((*ptr_current_location) - 1) * inflate;
        } else {
          *ptr_re = NA_INTEGER;
        }
        ptr_re++;
      }
      ptr_current_location++;
      if(ptr_current_location == current_location.end()){
        ptr_current_location = current_location.begin();
      }
    }
    
    // Prepare for next loop
    inflate = inflate * tmp;
    neach = neach * location_ii.size();
    
  }
  
  return(re);
}




/*** R

cat("Test loc2idx with small data set - validate\n")
x = array(1:8,c(2,2,2))
dim = dim(x)
locs = list(0:1L,1L, 0:3L)
target_dim = sapply(locs, length)
tmp = loc2idx(locs, dim); tmp
# validate in R
mfactor <- c(1, cumprod(dim))[seq_along(dim)]
scaled_loc <- lapply(seq_along(locs), function(ii){
  x <- as.integer(locs[[ii]])
  d <- dim[[ii]]
  x[x < 1 | x > d] <- NA 
  (x - 1) * mfactor[[ii]]
})
t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp, na.rm = TRUE)

# Performance check
cat("Test loc2idx with large data set - performance\n")
dim = c(300L, 200L, 600L, 100L)
locs = list(
  1:300L,
  1:200L,
  1:600L,
  1L
)
target_dim = sapply(locs, length)

m <- bench::mark({
  tmp = loc2idx(locs, dim);# tmp
  dim(tmp) = target_dim
})

m1 <- bench::mark({
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
})
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp)




# dim = as.integer(c(200,100,20)); x = array(seq_len(prod(dim)), dim); idx = sample(x, 1)
# # cpp_index_to_loc(idx+prod(dim), dim)
# 
# locs = lapply(dim, sample)
# system.time({
#   y1 = loc2idx(seq_len(prod(dim)), locs, dim, TRUE)
# })
# # which(x == idx, arr.ind = TRUE)
# system.time({
#   y2 = x[locs[[1]], locs[[2]], locs[[3]]]
# })
# 
# range(y1-y2)
cat("Test loc2idx with missing values - robust\n")
x = (1:5000000); pryr::object_size(x)
# arr2df(x, c(0L, 25L, 50L), 9L)
dim = c(50000L, 10L, 10L)
locs = list(
  as.integer(sample(100000L, 300) - 2000),
  as.integer(sample(12)-1),
  as.integer(sample(12)-1)
)
target_dim = sapply(locs, length)
tmp = loc2idx(locs, dim);# tmp
dim(tmp) = target_dim

# validate in R
mfactor <- c(1, cumprod(dim))[seq_along(dim)]
scaled_loc <- lapply(seq_along(locs), function(ii){
  x <- as.integer(locs[[ii]])
  d <- dim[[ii]]
  x[x < 1 | x > d] <- NA 
  (x - 1) * mfactor[[ii]]
})
t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp, na.rm = TRUE)

dim(x) = c(50000L, 10L, 10L)
a = locs[[1]]; a[(a<1) | (a >50000)] = NA
b = locs[[2]]; b[b<1 | b > 10] = NA
c = locs[[3]]; c[c<1 | c > 10] = NA
y2 <- x[a,b,c]
range(y2 - tmp, na.rm = TRUE)

y3 = tmp
sum(abs(is.na(y2) - is.na(y3)))
which(abs(is.na(y2) - is.na(y3)) > 0, arr.ind = TRUE)
range(y2 - y3, na.rm = T)

# s = sample(500000, size = 5, replace = TRUE)
# 
# v1 = cpp_load_lazyarray_base(f, c(2,3,1), c(-1L, as.integer(s)), 1); v1
# v1 = cpp_load_lazyarray_base(f, c(2,3,2), c(-1L, as.integer(s)), c(0,1)); v1

# cpp_fstmeta(f)

# system.time({
#   cpp_array_loc_to_index_homogeneous(as.integer(c(200, 500, 100)), list(
#     as.integer(sample(300)),
#     as.integer(sample(200)),
#     as.integer(sample(300))
#   ))
# })

# f= normalizePath("~/Desktop/junk/junk.fst", mustWork = FALSE)
# s = sample(500000, size = 5000000, replace = TRUE)
# system.time({
#   v1 = cpp_load_lazyarray_base(f, c(length(s), 10L), as.integer(s), 1:10)
# })
# system.time({
#   v2 = fst::read_fst(f, sprintf('V%d', 1:10), min(s), max(s))
#   v2[s - min(s) + 1,]
# })


# range(as.matrix(v2[s - min(s) + 1,] - v1))

# cpp_create_lazyarray(x, c(length(x) / 10,10), normalizePath(f, mustWork = FALSE), 13L, 100L, TRUE)
# file.info(f)
# # Sys.chmod(f, mode = "0777", use_umask = FALSE)
# # file.copy(f, '~/Desktop/junk/junk2.fst')
# 
# s = as.integer(sample(1e5, 200))
# ss = cpp_load_lazyarray(normalizePath('~/Desktop/junk/junk.fst'), 'V10', min(s), max(s))
# 
# t1 = Sys.time(); lapply(1:10, function(ii){
#   cpp_load_lazyarray(normalizePath('~/Desktop/junk/junk.fst'), sprintf('V%d', ii), min(s), max(s))
# }); Sys.time() - t1
# 
# t1 = Sys.time(); lapply(1:10, function(ii){
#   lapply(s, function(s1){
#     cpp_load_lazyarray(normalizePath('~/Desktop/junk/junk.fst'), sprintf('V%d', ii), s1, s1 + 1000L)
#     NULL
#   })
# }); Sys.time() - t1

*/
