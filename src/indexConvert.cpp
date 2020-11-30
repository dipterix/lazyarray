#include "indexConvert.h"
// [[Rcpp::plugins("cpp11")]]

#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
using namespace Rcpp; 

SEXP subsetIdx(Environment expr_env, NumericVector dim, bool pos_subscript){
  
  // // Get environment
  // Environment env(-1);
  // // get expr env to access declare (?)
  // env = env["expr"];
  
  SEXP dots = Rf_findVarInFrame(expr_env, R_DotsSymbol);
  
  SEXP i = expr_env["i"];
  
  // SEXP i = expr_env["i"];
  List location_idx = List::create();
  R_xlen_t ndims = dim.size();
  
  // mode: 
  // 0: x[i,j,k,l,...]
  // 1: x[i]
  // 2: x[]
  int subset_mode = 0;
  
  SEXP el;
  
  location_idx.push_front( R_MissingArg );
  R_xlen_t idx_size = 1;
  
  // used to estimate expected dimension
  int64_t total_length = std::accumulate(dim.begin(), dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  NumericVector target_dim = NumericVector(dim.begin(), dim.end());
  LogicalVector neg_subscr = LogicalVector(ndims, false);
  
  
  int64_t di;
  NumericVector sidx;
  NumericVector neg_sidx;
  
  for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots) ){
    
    if( idx_size > ndims ){
      stop("Incorrect dimension while subsetting an array");
    }
    
    el = CAR(dots);
    
    // el might be promise SEXP, if so, evaluate
    if ( TYPEOF(el) == PROMSXP ){
      // This is a promise, need to evaluate
      el = Rf_eval( PREXPR(el), PRENV( el ));
    }
    
    // current margin size
    di = dim[ idx_size ];
    
    // Check if el is missing value, this means 
    if( el == R_MissingArg ){
      
      neg_subscr[ idx_size ] = false;
      target_dim[ idx_size ] = di;
      
      location_idx.push_back( el );
      idx_size++;
      continue;
    }
    
    sidx = as<NumericVector>(el);
    
    neg_sidx = sidx[ !((is_na(sidx)) | (sidx >= 0)) ];
    
    
    if( neg_sidx.length() > 0 ){
      if( is_true( any( !((is_na(sidx)) | (sidx <= 0)) ) ) ){
        stop("only 0's may be mixed with negative subscripts");
      }
      neg_sidx = neg_sidx * (-1);
      sidx = sort_unique( neg_sidx );
      sidx = sidx[sidx <= di];
      if( pos_subscript ){
        NumericVector tmp = no_init( di - sidx.size() );
        NumericVector::iterator ptr_neg_sidx = tmp.begin();
        NumericVector::iterator ptr_sidx = sidx.begin();
        for(int64_t el = 1; (el <= di) && (ptr_neg_sidx != tmp.end()); el++){
          if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
            ptr_sidx++;
          } else {
            *ptr_neg_sidx++ = el;
          }
        }
        
        sidx = tmp;
        neg_subscr[ idx_size ] = false;
        target_dim[ idx_size ] = sidx.size();
      } else {
        neg_subscr[ idx_size ] = true;
        target_dim[ idx_size ] = di - sidx.size();
      }
      
    } else {
      
      if( is_true( any( !((is_na(sidx)) | (sidx <= di)) ) ) ){
        stop("incorrect number of dimensions");
      }
      
      sidx = sidx[ (is_na(sidx)) | (sidx > 0)];
      
      neg_subscr[ idx_size ] = false;
      target_dim[ idx_size ] = sidx.size();
    }
    
    
    location_idx.push_back( sidx );
    idx_size++;
  }
  
  if(location_idx.size() == 1){
    if(i == R_MissingArg){
      subset_mode = 2;
    } else {
      subset_mode = 1;
    }
  } else if (ndims != location_idx.size()){
    stop("Dimension mismatch while subseting an array");
  }
  
  // come back for the first i
  di = dim[ 0 ];
  int64_t expected_len = -1;
  if(subset_mode == 2){
    // x[] is called, target_dim is dim and neg_subscr are all false
    // no need to do anything
    
  } else if(i != R_MissingArg){
    sidx = as<NumericVector>( i );
    neg_sidx = sidx[ !((is_na(sidx)) | (sidx >= 0)) ];
    
    if( neg_sidx.length() > 0 ){
      if( is_true( any( !((is_na(sidx)) | (sidx <= 0)) ) ) ){
        stop("only 0's may be mixed with negative subscripts");
      }
      neg_sidx = neg_sidx * (-1);
      sidx = sort_unique(neg_sidx);
      neg_subscr[ 0 ] = true;
    } else {
      neg_subscr[ 0 ] = false;
    }
    
    
    if(subset_mode == 1) {
      // if subset_mode == 1,
      // i has value, subset is called as x[i]
      // target_dim will be ignored
      
      if(neg_subscr[ 0 ]){
        // need to make sure i is all valid - negative idx
        sidx = sidx[sidx <= total_length];
        
        if(pos_subscript){
          NumericVector tmp = no_init( total_length - sidx.size() );
          NumericVector::iterator ptr_neg_sidx = tmp.begin();
          NumericVector::iterator ptr_sidx = sidx.begin();
          for(int64_t el = 1; (el <= total_length) && (ptr_neg_sidx != tmp.end()); el++){
            if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
              ptr_sidx++;
            } else {
              *ptr_neg_sidx++ = el;
            }
          }
          
          sidx = tmp;
          neg_subscr[ 0 ] = false;
          target_dim[ 0 ] = sidx.size();
          expected_len = sidx.size();
        } else {
          location_idx[0] = sidx;
          expected_len = total_length - sidx.size();
        }
        
      } else {
        sidx = sidx[(is_na(sidx)) | (sidx > 0)];
        sidx[ (is_na(sidx)) | (sidx > total_length) ] = NA_REAL;
        expected_len = sidx.size();
      }
      
      location_idx[ 0 ] = sidx;
      
    } else {
      // x[i,j,k,l,...]
      
      if( neg_subscr[ 0 ] ){
        sidx = sidx[sidx <= di];
        if( pos_subscript ){
          NumericVector tmp = no_init( di - sidx.size() );
          NumericVector::iterator ptr_neg_sidx = tmp.begin();
          NumericVector::iterator ptr_sidx = sidx.begin();
          for(int64_t el = 1; (el <= di) && (ptr_neg_sidx != tmp.end()); el++){
            if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
              ptr_sidx++;
            } else {
              *ptr_neg_sidx++ = el;
            }
          }
          
          sidx = tmp;
          neg_subscr[ 0 ] = false;
          target_dim[ 0 ] = sidx.size();
        } else {
          sidx = sidx[sidx <= di];
          neg_subscr[ 0 ] = true;
          target_dim[ 0 ] = di - sidx.size();
        }
        
      } else {
        if( is_true( any( sidx > di ) ) ){
          stop("incorrect number of dimensions");
        }
        sidx = sidx[(is_na(sidx)) | (sidx > 0)];
        target_dim[ 0 ] = sidx.size();
      }
      
      location_idx[ 0 ] = sidx;
    }
    
  }
  
  if(expected_len < 0){
    expected_len = std::accumulate(target_dim.begin(), target_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  }
  
  
  
  return List::create(
    _["subset_mode"] = subset_mode,
    _["target_dimension"] = target_dim,
    _["expected_length"] = expected_len,
    _["location_indices"] = location_idx,
    _["negative_subscript"] = neg_subscr
  );
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
  IntegerVector sub_dim = IntegerVector(parent_dim.size());
  for(R_xlen_t dd = 0; dd < parent_dim.size(); dd++){
    SEXP subloc = locations[dd];
    if(subloc == R_MissingArg){
      sub_dim[dd] = parent_dim[dd];
    } else {
      sub_dim[dd] = Rf_xlength(subloc);
    }
  }
  // Total length to return
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  
  // Generate integer vector to be returned and assign dimension
  IntegerVector re(sub_size, 1);
  re.attr("dim") = sub_dim;
  
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
    IntegerVector location_ii;
    SEXP subloc_i = locations[ii];
    if(subloc_i == R_MissingArg){
      location_ii = seq_len(parent_dim[ii]);
    } else {
      location_ii = IntegerVector( subloc_i );
    }
    
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

NumericVector loc2idx2(List& locations, NumericVector& parent_dim){
  
  // Check whether parent_dim matches with location index size - validation
  R_xlen_t ndim = parent_dim.size();
  
  if( ndim != locations.size() ){
    stop("Dimension input wrong for `loc2idx2`");
  }
  
  // Get sub-dimension for location indexes (the dimension of returned value)
  NumericVector sub_dim = NumericVector(ndim);
  std::vector<std::vector<int64_t>> location_copy(ndim);
  
  for(R_xlen_t dd = 0; dd < ndim; dd++){
    SEXP subloc = locations[dd];
    if(subloc == R_MissingArg){
      sub_dim[dd] = parent_dim[dd];
      
      std::vector<int64_t> current_location(sub_dim[dd]);
      std::iota(current_location.begin(), current_location.end(), 1);
      location_copy[dd] = current_location;
    } else {
      sub_dim[dd] = Rf_xlength(subloc);
      location_copy[dd] = as<std::vector<int64_t>>(subloc);
    }
  }
  
  // Total length to return
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  
  // Generate integer vector to be returned and assign dimension
  NumericVector re(sub_size, 1);
  re.attr("dim") = sub_dim;
  
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
    std::vector<int64_t> current_location = location_copy[ii];
    
    // Assign invalid indexes to NA
    // The first needs to be is_na otherwise error will be raised: "can't subset using a logical vector with NAs"
    // current_location[ is_na(current_location) | current_location < 1 | current_location > tmp ] = NA_INTEGER;
    
    // write to re,
    // re += rep(current_location, nrepeat, neach)
    // if( neach > 1 ){
    //   current_location = Rcpp::rep_each(current_location, neach);
    // }
    // re += Rcpp::rep_len(current_location, sub_size);
    std::vector<int64_t>::iterator ptr_current_location = current_location.begin();
    
    for( NumericVector::iterator ptr_re = re.begin(); ptr_re != re.end(); ){
      if(ptr_current_location == current_location.end()){
        ptr_current_location = current_location.begin();
      }
      for(jj = 0; jj < neach; jj++){
        // if re[...] is not NA
        if(*ptr_re != NA_REAL && 
           
           // current_location is not NA
           *ptr_current_location != NA_REAL &&
           *ptr_current_location != NA_INTEGER64 &&
           
           // current_location is valid, i.e. current_location >= 1 & current_location <= tmp
           *ptr_current_location >= 1 &&
           
           *ptr_current_location <= tmp
           
        ){
          
          // index[[...]] += (locations[[ii]] - 1) * inflate
          *ptr_re += ((*ptr_current_location) - 1) * inflate;
        } else {
          *ptr_re = NA_REAL;
        }
        ptr_re++;
      }
      if(ptr_current_location != current_location.end()){
        ptr_current_location++;
      }
    }
    // Prepare for next loop
    inflate = inflate * tmp;
    neach = neach * current_location.size();
    
  }
  
  return(re);
}


std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim){
  
  // Check whether parent_dim matches with location index size - validation
  R_xlen_t ndim = parent_dim.size();
  
  if( ndim != Rf_xlength(locations) ){
    stop("Dimension input wrong for `loc2idx2`");
  }
  
  // Get sub-dimension for location indexes (the dimension of returned value)
  std::vector<int64_t> sub_dim(ndim);
  std::vector<std::vector<int64_t>> location_copy(ndim);
  
  for(R_xlen_t dd = 0; dd < ndim; dd++){
    SEXP subloc = VECTOR_ELT(locations, dd);
    if(subloc == R_MissingArg){
      sub_dim[dd] = parent_dim[dd];
      
      std::vector<int64_t> current_location(sub_dim[dd]);
      std::iota(current_location.begin(), current_location.end(), 1);
      location_copy[dd] = current_location;
    } else {
      sub_dim[dd] = Rf_xlength(subloc);
      location_copy[dd] = as<std::vector<int64_t>>(subloc);
    }
  }
  
  // Total length to return
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  
  // Generate integer vector to be returned and assign dimension
  std::vector<int64_t> re(sub_size, 1);
  
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
    std::vector<int64_t> current_location = location_copy[ii];
    
    // Assign invalid indexes to NA
    // The first needs to be is_na otherwise error will be raised: "can't subset using a logical vector with NAs"
    // current_location[ is_na(current_location) | current_location < 1 | current_location > tmp ] = NA_INTEGER;
    
    // write to re,
    // re += rep(current_location, nrepeat, neach)
    // if( neach > 1 ){
    //   current_location = Rcpp::rep_each(current_location, neach);
    // }
    // re += Rcpp::rep_len(current_location, sub_size);
    std::vector<int64_t>::iterator ptr_current_location = current_location.begin();
    
    for( std::vector<int64_t>::iterator ptr_re = re.begin(); ptr_re != re.end(); ){
      if(ptr_current_location == current_location.end()){
        ptr_current_location = current_location.begin();
      }
      for(jj = 0; jj < neach; jj++){
        // if re[...] is not NA
        if(*ptr_re != NA_REAL && 
           *ptr_re != NA_INTEGER64 && 
           
           // current_location is not NA
           *ptr_current_location != NA_REAL &&
           *ptr_current_location != NA_INTEGER64 &&
           
           // current_location is valid, i.e. current_location >= 1 & current_location <= tmp
           *ptr_current_location >= 1 &&
           
           *ptr_current_location <= tmp
           
        ){
          
          // index[[...]] += (locations[[ii]] - 1) * inflate
          *ptr_re += ((*ptr_current_location) - 1) * inflate;
        } else {
          *ptr_re = NA_REAL;
        }
        ptr_re++;
      }
      if(ptr_current_location != current_location.end()){
        ptr_current_location++;
      }
    }
    // Prepare for next loop
    inflate = inflate * tmp;
    neach = neach * current_location.size();
    
  }
  
  return(re);
}

List extractSlices(SEXP listOrEnv, const R_xlen_t& ndims){
  switch(TYPEOF(listOrEnv)) {
  case ENVSXP: {
    List sliceIdx = List::create();
    
    try {
      Rcpp::Environment env = listOrEnv;
      sliceIdx.push_back( env.find("i") );
    } catch (...){}
    
      // i is missing, scenario 1
    SEXP dots = Rf_findVarInFrame(listOrEnv, R_DotsSymbol);
    R_xlen_t idx_size = 0;
    for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots), idx_size++ ){
      if(idx_size >= ndims){
        stop("Incorrect subscript dimensions, required: 0, 1, ndim.");
      }
      sliceIdx.push_back(CAR(dots));
    }
    return sliceIdx;
  }
  case VECSXP:
    return as<List>(listOrEnv);
  default:
    Rcpp::stop("Input `listOrEnv` must be either a list of indices or an environment");
  }
}

List parseSlices(SEXP listOrEnv, const std::vector<int64_t>& dim, bool pos_subscript){
  tok("S parseSlices");
  ParsedIndex *subparsed = new ParsedIndex(listOrEnv, dim, pos_subscript);
  List res = subparsed->asList();
  delete subparsed;
  tok("E parseSlices");
  return res;
}

List parseAndScheduleBlocks2(SEXP listOrEnv, NumericVector dim, bool forceSchedule){
  ParsedIndex* subparsed = parseAndScheduleBlocks(listOrEnv, as<std::vector<int64_t>>(dim), forceSchedule);
  List res = subparsed->asList();
  delete subparsed;
  return res;
}

ParsedIndex* parseAndScheduleBlocks(SEXP listOrEnv, const std::vector<int64_t>& dim, bool forceSchedule, int64_t hint){
  tok("S parseAndScheduleBlocks");
  
  
  // parse index
  ParsedIndex* subparsed = new ParsedIndex(listOrEnv, dim, true);
  
  // subparsed
  // schedule index
  
  if(subparsed->subset_mode == LASUBMOD_MULTI){
    List re = PROTECT(subparsed->asList());
    ScheduledIndex* schedule = new ScheduledIndex(Shield<SEXP>(wrap(re["location_indices"])), dim, forceSchedule, hint);
    UNPROTECT(1);
    subparsed->schedule = schedule;
  }
  
  
  tok("E parseAndScheduleBlocks");
  return subparsed;
}



SEXP reshapeOrDrop(SEXP x, SEXP reshape, bool drop){
  // SEXP reshape, bool drop = false
  // if reshape is not null, drop is ignored
  if(reshape == R_NilValue && !drop){
    return x;
  }
  
  if(reshape == R_NilValue && drop){
    dropDimension(x);
    return x;
  }
  
  // reshape has length, hence need to check dimension length
  
  // subset_mode=0 => x[i,j,k]
  // subset_mode=1 => x[i]
  // subset_mode=2 => x[]
  SEXP reshape_alt = reshape;
  int n_protected = 0;
  if(TYPEOF(reshape) != REALSXP){
    reshape_alt = PROTECT(Rf_coerceVector(reshape_alt, REALSXP));
    n_protected++;
  }
  const int64_t reshape_length = prod2(reshape_alt, false);
  const int64_t expected_length = Rf_xlength(x);
  
  if(reshape_length == NA_INTEGER64 || reshape_length != expected_length){
    warning("`reshape` has different length than expected. Request to reshape dimension is ignored.");
  } else {
    if(Rf_xlength(reshape_alt) >= 2){
      Rf_setAttrib(x, wrap("dim"), reshape_alt);
    } else {
      Rf_setAttrib(x, wrap("dim"), R_NilValue);
    }
  }
  
  if(n_protected > 0){
    UNPROTECT(n_protected);
  }
  
  return x;
}

