#include "loader2.h"

// This file is too long, split into multiple files
#include "loader2double.h"

using namespace Rcpp;

SEXP lazySubset(StringVector& files, Environment& env, NumericVector& dim, SEXP samp){
  
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
  
  return res;
}
