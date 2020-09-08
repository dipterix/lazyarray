#include "utils.h"
#include "loader2.h"

// This file is too long, split into multiple files
#include "loader2ext.h"

using namespace Rcpp;



SEXP lazySubset(StringVector& files, Environment& env, NumericVector& dim, SEXP samp, SEXP reshape, bool drop){
  
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  
  R_xlen_t npart = *(dim.end() - 1);
  
  if( npart != files.size() ){
    stop("Partition size does not match with file counts.");
  }
  
  List subparsed = subsetIdx(env, dim, true);
  
  checkUserInterrupt();
  
  SEXP res = R_NilValue;
  
  switch(TYPEOF(samp)){
  case REALSXP: 
    res = lazySubset_double(files, dim, subparsed);
    break;
  case INTSXP:
    res = lazySubset_integer(files, dim, subparsed);
    break;
  case STRSXP:
  case CHARSXP:
    res = lazySubset_character(files, dim, subparsed);
    break;
  case CPLXSXP:
    res = lazySubset_complex(files, dim, subparsed);
    break;
  default:
    stop("Unknown data type: only numeric, integer, character, and complex arrays are supported.");
  }
  
  // SEXP reshape, bool drop = false
  // if reshape is not null, drop is ignored
  if(reshape == R_NilValue && !drop){
    return res;
  }
  
  if(reshape == R_NilValue && drop){
    dropDimension(res);
    return res;
  }
  
  // reshape has length, hence need to check dimension length
  
  // subset_mode=0 => x[i,j,k]
  // subset_mode=1 => x[i]
  // subset_mode=2 => x[]
  const int64_t expected_length = subparsed["expected_length"];
  SEXP reshape_alt = reshape;
  if(TYPEOF(reshape) != REALSXP){
    reshape_alt = Rf_coerceVector(reshape_alt, REALSXP);
  }
  const int64_t reshape_length = prod2(reshape_alt, false);
  
  if(reshape_length == NA_INTEGER64 || reshape_length != expected_length){
    warning("`reshape` has different length than expected. Request to reshape dimension is ignored.");
  } else {
    if(Rf_xlength(reshape_alt) >= 2){
      Rf_setAttrib(res, wrap("dim"), reshape_alt);
    } else {
      Rf_setAttrib(res, wrap("dim"), R_NilValue);
    }
  }
  
  return res;
}
