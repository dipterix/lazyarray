#include "saver2ext.h"

#include "common.h"
#include "fstWrapper.h"
#include "indexConvert.h"
using namespace Rcpp;


void writeFstPartition_double(const Rcpp::NumericVector& values, const Rcpp::StringVector& files, 
                              const Rcpp::NumericVector& dim, const Rcpp::List& subparsed,
                              int compression, bool uniformEncoding){
  // scheduled
  // subparsed = parseSlices(SEXP listOrEnv, NumericVector dim)
  
  R_xlen_t value_len = values.size();
  
  if( value_len == 0 ){
    stop("Value length is 0");
  }
  R_xlen_t ndims = dim.size();
  const int64_t expected_length = subparsed["expected_length"];
  
  if( expected_length % value_len != 0 ){
    if(value_len == 0){
      stop("Value length is 0");
    } else if(value_len != 1){
      stop("number of items to replace is not a multiple of replacement length. Make sure subscripts contain no 0, and item size is consistent with replacement length.");
    }
  }
  
  const int subset_mode = subparsed["subset_mode"];
  const List location_indices = subparsed["location_indices"];
  
  
  // make index when subset_mode is 0
  bool make_idx = false;
  if(subset_mode == 0){
    for(R_xlen_t ii=0; ii<ndims; ii++ ){
      if(location_indices[ii] != R_MissingArg){
        make_idx = true;
        break;
      }
    }
  } else if(subset_mode == 1){
    stop("Single subscript assign (x[i]<-value) has not yet implemented. Please use full subscript (x[,...,i]<-value) or no subscript (x[]<-value)");
  }
  
  std::vector<int64_t> row_idx(0);
  std::vector<int64_t> dim_part = std::vector<int64_t>(dim.begin(), dim.end() - 1);
  int64_t expected_nrows = std::accumulate(dim_part.begin(), dim_part.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  if( make_idx ){
    const List location_indices_part = List(location_indices.begin(), location_indices.end()-1);
    row_idx = loc2idx3(wrap(location_indices_part), dim_part);
  }
  
  SEXP partition_loc = location_indices[ndims - 1];
  NumericVector partition_loc_alt;
  if(partition_loc == R_MissingArg){
    partition_loc_alt = seq_len(dim[ndims - 1]);
  } else {
    partition_loc_alt = as<NumericVector>(partition_loc);
  }
  R_xlen_t nparts = partition_loc_alt.size();
  
  NumericVector v = as<NumericVector>(values);
  NumericVector::iterator ptr_v = v.begin();
  R_xlen_t ii_v = 0;
  StringVector cname = {"V1"};
  NumericVector partition;
  
  for(R_xlen_t part_ii = 0; part_ii < nparts; part_ii++ ){
    
    
    int64_t part = partition_loc_alt[part_ii];
    if(part == NA_REAL || part == NA_INTEGER64 || part < 1 || part > files.size()){
      // skip
      ii_v += expected_nrows;
      ii_v = ii_v % value_len;
      ptr_v = v.begin() + ii_v;
    }
    
    String fstfile = files[part - 1];
    
    // if not make index x[] <- v or x[,,i] <- v or x[,,,] <- v
    
    
    if(make_idx){
      if(checkFstMeta(fstfile, expected_nrows, cname)){
        List tmp = fstRetrieve(fstfile, wrap(cname), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        partition = as<NumericVector>(tmp["V1"]);
      } else {
        partition = NumericVector(expected_nrows, NA_REAL);
      }
      
      std::vector<int64_t>::iterator ptr_row_idx = row_idx.begin();
      for(; ptr_row_idx != row_idx.end(); ptr_row_idx++){
        partition[(*ptr_row_idx)-1] = *ptr_v;
        ptr_v++; ii_v++;
        if(ptr_v == v.end()){
          ptr_v = v.begin();
          ii_v = 0;
        }
      }
      
    } else {
      // value_len
      partition = NumericVector(expected_nrows);
      for(NumericVector::iterator ptr_partition = partition.begin(); ptr_partition != partition.end(); ptr_partition++){
        *ptr_partition = *ptr_v;
        ptr_v++; ii_v++;
        if(ptr_v == v.end()){
          ptr_v = v.begin();
          ii_v = 0;
        }
      }
      
    }
    
    // write to file
    List tbl = List::create(_["V1"] = partition);
    fstStore(fstfile, tbl, wrap(compression), wrap(uniformEncoding));
    
  }
  
  
}



/*** R
x <- lazyarray:::as.lazyarray(array(rep(1,27), rep(3,3)))
unlink(x$get_partition_fpath(3))
res <- lazyarray:::parseAndScheduleBlocks(list(lazyarray:::get_missing_value(),1,1:3), dim(x))
writeFstPartition_double(1.1, x$get_partition_fpath(), dim(x), res, 50L, TRUE)
x[]
x[,1,]
*/
