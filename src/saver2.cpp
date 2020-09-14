#include "saver2.h"

#include "common.h"
#include "utils.h"
#include "saver2ext.h"
#include "indexConvert.h"
using namespace Rcpp;

// SEXP writeFstPartition_double(const Rcpp::NumericVector& values, const Rcpp::StringVector& files, 
//                               const Rcpp::NumericVector& dim, const Rcpp::List& subparsed,
//                               SEXP compression, SEXP uniformEncoding)
// SEXP subsetFST(Rcpp::StringVector& files, SEXP listOrEnv, Rcpp::NumericVector& dim, 
//                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false)

SEXP subsetAssignFST(const SEXP values, const std::string& file, SEXP listOrEnv,
                     const NumericVector& dim, const SEXPTYPE& dtype,
                     int compression, bool uniformEncoding){
  List subparsed = parseSlices(listOrEnv, dim);
  // const int subset_mode = subparsed["subset_mode"];
  
  // // If subset_mode == 1 - x[i]
  // if(subset_mode == LASUBMOD_SINGLE){
  //   List location_indices = subparsed["location_indices"];
  //   
  //   
  // }
  std::string file_alt = as_dirpath(file);
  
  switch(dtype){
  case REALSXP:
    writeFstPartition_double(as<NumericVector>(values), file_alt, dim, subparsed, compression, uniformEncoding);
    break;
  default:
    stop("Unknown data type, only int(13), double(14), complex(15), string(16) are allows");
  }
  
  return R_NilValue;
  
}

  
/*** R
subsetAssignFST(1.1, x$get_partition_fpath(), dim(x), res, 14L, 50L, TRUE)
x <- lazyarray:::as.lazyarray(array(rep(1,27), rep(3,3)))
unlink(x$get_partition_fpath(3))

a = function(value, ...){
  subsetAssignFST(value, x$get_partition_fpath(), environment(),
                  dim(x), 14L, 50L, TRUE)
}

res <- lazyarray:::parseAndScheduleBlocks(list(lazyarray:::get_missing_value(),1,1:3), dim(x))
writeFstPartition_double(1.1, x$get_partition_fpath(), dim(x), res, 50L, TRUE)
x[]
x[,1,]

*/
