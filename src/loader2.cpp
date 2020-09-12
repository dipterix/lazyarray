#include "loader2.h"
// This file is too long, split into multiple files
#include "loader2ext.h"

#include "common.h"
#include "utils.h"
#include "indexConvert.h"


using namespace Rcpp;

SEXP subsetFST(StringVector& files, SEXP listOrEnv, NumericVector& dim, SEXPTYPE dtype, SEXP reshape, bool drop){
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  
  R_xlen_t npart = *(dim.end() - 1);
  
  if( npart != files.size() ){
    stop("Partition size does not match with file counts.");
  }
  
  const List subparsed = parseAndScheduleBlocks(listOrEnv, dim);
  
  checkUserInterrupt();
  
  SEXP res = R_NilValue;
  
  switch(dtype){
  case REALSXP: 
    res = subsetFST_double(files, dim, subparsed);
    break;
  case INTSXP:
    res = subsetFST_integer(files, dim, subparsed);
    break;
  case STRSXP:
  case CHARSXP:
    res = subsetFST_character(files, dim, subparsed);
    break;
  case CPLXSXP:
    res = subsetFST_complex(files, dim, subparsed);
    break;
  default:
    stop("Unknown data type: only numeric, integer, character, and complex arrays are supported - provided SEXPTYPE: " + std::to_string(dtype));
  }
  reshapeOrDrop(res, reshape, drop); 
  return res;
}
