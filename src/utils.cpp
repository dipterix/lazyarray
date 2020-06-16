
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
Rcpp::List cpp_array_to_list_template(T x, Rcpp::IntegerVector cutoff){
  
  // Rcpp::Timer _rcpp_timer;
  // _rcpp_timer.step("enter cpp_array_to_list_integer");
  
  if(Rcpp::max(cutoff) > x.size()){
    Rcpp::stop("Index set exceed max length of input.");
  } else if (Rcpp::min(cutoff) < 0){
    Rcpp::stop("Index set less than 1 is dis-allowed.");
  }
  
  Rcpp::List re = Rcpp::List::create();
  I ptr = x.begin() + cutoff[0];
  I ptr2 = ptr;
  Rcpp::String colname;
  for( R_xlen_t ii = 0; ii < cutoff.size() - 1; ii++ ){
    ptr2 += cutoff[ii + 1] - cutoff[ii];
    colname = "V" + std::to_string(ii + 1);
    
    const I p1 = ptr;
    const I p2 = ptr2;
    
    re.push_back(Armor<T>(T(p1, p2)), colname);
    ptr = ptr2;
    // _rcpp_timer.step("split-" + std::to_string(ii));
  }
  
  // _rcpp_timer.step("split-finished");
  
  // if( LAZYARRAY_DEBUG ){
  //   
  //   NumericVector _res(_rcpp_timer);
  //   _res = _res / 1000000.0;
  //   Rcpp::print(_res);
  // }
  
  return re;
}


Rcpp::List cpp_array_to_list_complex(ComplexVector x, Rcpp::IntegerVector cutoff){
  
  // Rcpp::Timer _rcpp_timer;
  // _rcpp_timer.step("enter cpp_array_to_list_integer");
  
  if(Rcpp::max(cutoff) > x.size()){
    Rcpp::stop("Index set exceed max length of input.");
  } else if (Rcpp::min(cutoff) < 0){
    Rcpp::stop("Index set less than 1 is dis-allowed.");
  }
  
  Rcpp::List re = Rcpp::List::create();
  ComplexVector::iterator ptr = x.begin() + cutoff[0];
  ComplexVector::iterator ptr2 = ptr;
  Rcpp::String colname;
  for( R_xlen_t ii = 0; ii < cutoff.size() - 1; ii++ ){
    ptr2 += cutoff[ii + 1] - cutoff[ii];
    const ComplexVector tmp(ptr, ptr2);
    
    colname = "V" + std::to_string(ii + 1) + "R";
    re.push_back(Armor<NumericVector>(Rcpp::Re(tmp)), colname);
    
    colname = "V" + std::to_string(ii + 1) + "I";
    re.push_back(Armor<NumericVector>(Rcpp::Im(tmp)), colname);
    
    ptr = ptr2;
    // _rcpp_timer.step("split-" + std::to_string(ii));
  }
  
  // _rcpp_timer.step("split-finished");
  
  // if( LAZYARRAY_DEBUG ){
  //   
  //   NumericVector _res(_rcpp_timer);
  //   _res = _res / 1000000.0;
  //   Rcpp::print(_res);
  // }
  
  return re;
}


Rcpp::List cpp_array_to_list(SEXP &x, IntegerVector &cutoff){
  // User explicitly tells which storage type of x should be
  // 9	CHARSXP	internal character strings
  // 10	LGLSXP	logical vectors
  // 13	INTSXP	integer vectors
  // 14	REALSXP	numeric vectors
  // 15	CPLXSXP	complex vectors
  // 16	STRSXP	character vectors
  // 24	RAWSXP	raw vector
  // 
  Rcpp::List re; 
  switch (TYPEOF(x)) {
  case STRSXP:
  case CHARSXP:
    re = cpp_array_to_list_template<CharacterVector, CharacterVector::iterator>(x, cutoff);
    break;
  case LGLSXP:
    re = cpp_array_to_list_template<LogicalVector, LogicalVector::iterator>(x, cutoff);
    break;
  case INTSXP:
    re = cpp_array_to_list_template<IntegerVector, IntegerVector::iterator>(x, cutoff);
    break;
  case REALSXP:
    re = cpp_array_to_list_template<NumericVector, NumericVector::iterator>(x, cutoff);
    break;
  case CPLXSXP:
    re = cpp_array_to_list_complex(x, cutoff);
    break;
  default:
    Rcpp::stop("Unsupported data type. Only logical, numeric, complex, character types are supported.");
  }
  return re;
}

// TO be depricated
IntegerVector cpp_array_loc_to_index_homogeneous(IntegerVector dim, List locations){
  // make sure indices are list of integervectors
  
  R_xlen_t ndims = dim.size();
  R_xlen_t ii = 0;
  if(ndims != locations.size()){
    stop("Dimention(" + std::to_string(ndims) +
      ") doesn't agree with locations(" + std::to_string(locations.size()) +
      ") in lengths");
  }
  // calculate multiply-factors
  std::vector<R_xlen_t> mfct(ndims, 1);
  std::vector<R_xlen_t> sfct(ndims, 1);
  IntegerVector target_dim(ndims);
  R_xlen_t res_len = 1;
  std::vector<IntegerVector> loc_copy(ndims);
  for( ; ii < ndims; ii++ ){
    loc_copy[ii] = locations[ii];
    target_dim[ii] = loc_copy[ii].size();
    res_len *= target_dim[ii];
    if( ii < ndims - 1 ){
      mfct[ii + 1] = dim[ii] * mfct[ii];
      sfct[ii + 1] = target_dim[ii] * sfct[ii];
    }
  }
  
  IntegerVector re = IntegerVector(res_len, 0);
  IntegerVector loc;
  R_xlen_t step, multi;
  IntegerVector::iterator ptr1, ptr2, ptr_loc;
  R_xlen_t count = 0;
  
  for( ii = 0; ii < ndims; ii++ ){
    loc = loc_copy[ii];
    step = sfct[ii];
    multi = mfct[ii];
    ptr2 = ptr1 = re.begin();
    count = 0;
    Rcout<<step<<"\n";
    while(ptr2 != re.end()){
      for(ptr_loc = loc.begin(); ptr_loc != loc.end(); ptr_loc++ ){
        ptr2 = ptr1 + step;
        count += step;
        
        // security, make sure ptr2 is valid
        if( count > re.size() ){
          break;
        }
        std::fill(ptr1, ptr2, *ptr_loc * multi);
        ptr1 = ptr2;
        
      }
    }
  }
  
  re.attr("dim") = target_dim;
  
  return re;
  
}


void c_index_to_loc(R_xlen_t *loc, const R_xlen_t& idx, R_xlen_t *dim, const R_xlen_t& ndim, const bool& allow_overflow){
  R_xlen_t res = idx - 1;
  for( R_xlen_t ii = 0; ii < ndim; ii++, loc++, dim++ ){
    if( ii == ndim - 1 && allow_overflow ){
      *loc = (res + 1);
    } else {
      *loc = (res % (*dim)) + 1;
      res = (res - *loc + 1) / *dim;
    }
  }
}


IntegerVector cpp_index_to_index(IntegerVector& idx, List& locations, IntegerVector& parent_dim){
  // 
  R_xlen_t ndim = parent_dim.size();
  R_xlen_t ii = 0;
  if( ndim != locations.size() ){
    stop("Dimension input wrong for `cpp_index_to_index`");
  }
  IntegerVector re(idx.size(), 1);
  
  // int loc[ndim];
  R_xlen_t *loc = (R_xlen_t *) malloc (ndim * sizeof(R_xlen_t));
  
  // 1. Copy dimension to dim_cpy[]
  // 2. Calculate multiply-factor
  // 3. copy locations
  R_xlen_t *dim_cpy = (R_xlen_t *) malloc (ndim * sizeof(R_xlen_t));
  R_xlen_t *dim_fct = (R_xlen_t *) malloc (ndim * sizeof(R_xlen_t));
  R_xlen_t *tmp_p = dim_cpy;
  R_xlen_t *tmp_p_alt = dim_fct;
  
  std::vector<std::vector<int>> loc_pos(ndim);
  std::vector<std::vector<int>>::iterator p_loc_pos = loc_pos.begin();
  IntegerVector::iterator ptr, ptr_alt, ptr_alt2;
  
  for( ii = 0 ; ii < ndim; p_loc_pos++, tmp_p++, tmp_p_alt++, ii++ ){
    *p_loc_pos = as<std::vector<int>>(locations[ii]);
    *tmp_p = (*p_loc_pos).size();
    if( ii == 0 ){
      *tmp_p_alt = 1;
    } else {
      *tmp_p_alt = *(tmp_p_alt - 1) * parent_dim[ii-1];
    }
  }
  int tmp_value = 0;
  
  for( ptr = idx.begin(), ptr_alt = re.begin(); ptr != idx.end(); ptr++, ptr_alt++ ){
    c_index_to_loc(loc, *ptr, dim_cpy, ndim, true);
    // Rcout << loc[0] <<' ' << loc[1]<<' '<< loc[2] << "\n";
    
    for(
      tmp_p = loc, p_loc_pos = loc_pos.begin(), tmp_p_alt = dim_fct, ptr_alt2 = parent_dim.begin();
      p_loc_pos != loc_pos.end();
      p_loc_pos++, tmp_p++, tmp_p_alt++, ptr_alt2++
    ) {
      if( *ptr_alt != NA_INTEGER ){
        tmp_value = (*p_loc_pos)[*(tmp_p) - 1];
        if( tmp_value <= 0 || tmp_value > *ptr_alt2 ){
          *ptr_alt = NA_INTEGER;
        } else {
          *ptr_alt += (tmp_value - 1) * (*tmp_p_alt);
        }
      }
      
    }
  }
  
  free(loc);
  loc = NULL;
  
  free(dim_fct);
  dim_fct = NULL;
  
  free(dim_cpy);
  dim_cpy = NULL;
  tmp_p = NULL;
  tmp_p_alt = NULL;
  
  return(re);
}




/*** R
# dim = as.integer(c(200,100,20)); x = array(seq_len(prod(dim)), dim); idx = sample(x, 1)
# # cpp_index_to_loc(idx+prod(dim), dim)
# 
# locs = lapply(dim, sample)
# system.time({
#   y1 = cpp_index_to_index(seq_len(prod(dim)), locs, dim, TRUE)
# })
# # which(x == idx, arr.ind = TRUE)
# system.time({
#   y2 = x[locs[[1]], locs[[2]], locs[[3]]]
# })
# 
# range(y1-y2)

x = (1:5000000); pryr::object_size(x)
# cpp_array_to_list(x, c(0L, 25L, 50L), 9L)
dim = c(50000L, 10L, 10L)
locs = list(
  as.integer(sample(100000L, 300) - 2000),
  as.integer(sample(12)-1),
  as.integer(sample(12)-1)
)
target_dim = sapply(locs, length)
tmp = cpp_index_to_index(seq_len(prod(target_dim)), locs, dim);# tmp
dim(tmp) = target_dim

# a = lazyarray:::cpp_array_to_list(x, c(2L, 4L, 10L, length(x)), 9L)
# a
f= normalizePath("~/Desktop/junk/junk.fst", mustWork = FALSE)
y1 <- cpp_load_lazyarray(f, locs, dim, 9L)

dim(x) = c(50000L, 10L, 10L)
a = locs[[1]]; a[(a<1) | (a >50000)] = NA
b = locs[[2]]; b[b<1 | b > 10] = NA
c = locs[[3]]; c[c<1 | c > 10] = NA
y2 <- x[a,b,c]

y3 = tmp
sum(abs(is.na(y2) - is.na(y3)))
which(abs(is.na(y2) - is.na(y3)) > 0, arr.ind = TRUE)
range(y2 - y3, na.rm = T)

sum(abs(is.na(y2) - is.na(y1)))
which(abs(is.na(y2) - is.na(y1)) > 0, arr.ind = TRUE)

range(y2 - y1, na.rm = T)
dif = y2 - y1
dif[is.na(dif)] = 0
which(dif != 0, arr.ind = TRUE)
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
