#include "loader2.h"
// This file is too long, split into multiple files
#include "loader2ext.h"

#include "common.h"
#include "utils.h"
#include "indexConvert.h"


using namespace Rcpp;

SEXP subsetFSTBare(const std::string& rootPath, const List& subparsed, const NumericVector& dim, const SEXPTYPE& dtype) {
  tok("S subsetFSTBare");
  SEXP res = R_NilValue;
  switch(dtype){
  case REALSXP: 
    res = subsetFST_double(rootPath, dim, subparsed);
    break;
  case INTSXP:
    res = subsetFST_integer(rootPath, dim, subparsed);
    break;
  case STRSXP:
  case CHARSXP:
    res = subsetFST_character(rootPath, dim, subparsed);
    break;
  case CPLXSXP:
    res = subsetFST_complex(rootPath, dim, subparsed);
    break;
  default:
    stop("Unknown data type: only numeric, integer, character, and complex arrays are supported - provided SEXPTYPE: " + std::to_string(dtype));
  }
  tok("E subsetFSTBare");
  return res;
}

SEXP subsetFST(const std::string& rootPath, SEXP listOrEnv, const NumericVector& dim, SEXPTYPE dtype, SEXP reshape, bool drop){
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  const std::string rootPath_alt = as_dirpath(rootPath);
  
  const List subparsed = parseAndScheduleBlocks(listOrEnv, dim);
  
  R_CheckUserInterrupt();
  
  SEXP res = subsetFSTBare(rootPath_alt, subparsed, dim, dtype);
  
  reshapeOrDrop(res, reshape, drop); 
  return res;
}
