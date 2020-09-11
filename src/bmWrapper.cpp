#include <Rcpp.h>
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <numeric>

#include "common.h"
#include "utils.h"
#include "indexConvert.h"
using namespace Rcpp;

// Logic for BigColSums.
template <typename T>
SEXP subsetBM_double(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat, List idxParsed) {
  
  // Create the vector we'll store the column sums in.
  // NumericVector colSums(pMat->ncol());
  // for (size_t i=0; i < pMat->ncol(); ++i)
  //   colSums[i] = std::accumulate(mat[i], mat[i]+pMat->nrow(), 0.0);
  // return colSums;
  
  // int subset_mode = idxParsed["subset_mode"];
  int64_t expected_length = idxParsed["expected_length"];
  SEXP target_dimension = idxParsed["target_dimension"];
  List location_indices = idxParsed["location_indices"];
  List schedule = idxParsed["schedule"];
  int64_t partition_counts = schedule["partition_counts"];
  std::vector<int64_t> partition_index = schedule["partition_index"];
  std::vector<int64_t> schedule_dimension = schedule["schedule_dimension"];
  std::vector<int64_t> block_dimension = schedule["block_dimension"];
  std::vector<int64_t> block_prod_dim = schedule["block_prod_dim"];
  bool block_indexed = schedule["block_indexed"];
  // REALSXP
  SEXP block_schedule = schedule["block_schedule"];
  // int64_t block_schedule_start = schedule["block_schedule_start"];
  // int64_t block_schedule_end = schedule["block_schedule_end"];
  int64_t block_length = schedule["block_length"];
  int64_t block_expected_length = schedule["block_expected_length"];
  std::vector<int64_t> schedule_index = schedule["schedule_index"];
  int64_t block_ndims = schedule["block_ndims"];
  SEXP block_location = schedule["block_location"];
  // return wrap(idxParsed);
  
  // print(block_schedule);
  // stop("");
  
  SEXP re = PROTECT(Rf_allocVector(REALSXP, expected_length));
  double *ptr_block_schedule = REAL(block_schedule);
  double *ptr_re = REAL(re);
  int64_t blocks_per_part = schedule_index.size();
  int64_t block_id = 0;
  
  int64_t rowIdx;
  
  // for each partition (column)
  for(int64_t coln = 0; coln < partition_counts; coln++){
    // get starting point
    int64_t column_ii = partition_index[coln];
    
    if(column_ii == NA_REAL || column_ii == NA_INTEGER64){
      double *ptr_alt = ptr_re + blocks_per_part * block_expected_length;
      for(; ptr_re != ptr_alt; ptr_re++ ){
        *ptr_re = NA_REAL;
      }
      continue;
    }
    
    T *rowAccessor = (T *)(mat[column_ii - 1]);
    
    for(int64_t block = 0; block < blocks_per_part; block++ ){
      block_id = *(schedule_index.begin() + block);
      
      if(block_id == NA_REAL || block_id == NA_INTEGER64){
        // block should be NA
        double *ptr_alt = ptr_re + block_expected_length;
        for(; ptr_re != ptr_alt; ptr_re++ ){
          *ptr_re = NA_REAL;
        }
        continue;
      }
      
      // Rcout << ((block_id - 1) * block_length) << " ";//<< "\n";
      T *blockAccessor = rowAccessor + (block_id - 1) * block_length;
      ptr_block_schedule = REAL(block_schedule);
      
      
      if( block_indexed ){
        for(int64_t ii = 0; ii < block_expected_length; ii++ ){
          rowIdx = *(ptr_block_schedule + ii);
          if(rowIdx == NA_REAL || rowIdx == NA_INTEGER64){
            *ptr_re++ = NA_REAL;
          } else {
            // Rcout << rowIdx << "\n";
            *ptr_re++ = (double)(*(blockAccessor + (rowIdx - 1)));
          }
          expected_length--;
        }
      } else{
        for(int64_t ii = 0; ii < block_expected_length; ii++ ){
          
          int64_t mod;
          int64_t rest = ii;
          int64_t sub_index = 0;
          int64_t subblock_dim_ii;
          int64_t tmp;
          // print(wrap(ii));
          // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
          for(int64_t di = 0; di < block_ndims; di++ ){
            expected_length--;
            if(sub_index != NA_INTEGER64){
              subblock_dim_ii = *(block_dimension.begin() + di);
              mod = rest % subblock_dim_ii;
              rest = (rest - mod) / subblock_dim_ii;
              // get di^th margin element mod
              // block_location[di][mod]
              SEXP location_ii = VECTOR_ELT(block_location, di);
              if(location_ii == R_MissingArg){
                // index[di]
                tmp = mod;
              } else {
                // index[di]
                // location_ii starts from 1 but we need starting from 0
                tmp = *(REAL(location_ii) + mod) - 1;
              }
              
              if(tmp == NA_REAL || tmp == NA_INTEGER64){
                sub_index = NA_INTEGER64;
              } else {
                sub_index += *(block_prod_dim.begin() + di) * tmp;
              }
            }
            
            
          }
          
          if( sub_index == NA_INTEGER64 ) {
            *(ptr_re + ii) = NA_REAL;
          } else {
            // sub_index = sub_index + 1 - subblock_min;
            *(ptr_re + ii) = (double)(*(blockAccessor + sub_index));
          }
        }
      }
    }
    
    
  }
  
  
  Rf_setAttrib(re, wrap("dim"), target_dimension);
  
  UNPROTECT(1);
  return re;
}

// Dispatch function for BigColSums
//
// [[Rcpp::export]]
SEXP subsetBM(SEXP pBigMat, SEXP listOrEnv, NumericVector dim, SEXP reshape = R_NilValue, bool drop = false) {
  // List parseAndScheduleBlocks(SEXP listOrEnv, NumericVector dim)
  // First we have to tell Rcpp what class to use for big.matrix objects.
  // This object stores the attributes of the big.matrix object passed to it
  // by R.
  XPtr<BigMatrix> xpMat(pBigMat);
  R_xlen_t ndims = dim.size();
  stopIfNot(ndims >= 2 && (xpMat->ncol()) == dim[ndims - 1],
            "c++ function `subsetBM`: size of dimension must >= 2 and the last dimension must be bigmatrix column counts"
  );
  
  List idxParsed = parseAndScheduleBlocks(listOrEnv, dim);
  
  SEXP res;
  
  // To access values in the big.matrix, we need to create a MatrixAccessor
  // object of the appropriate type. Note that in every case we are still
  // returning a NumericVector: this is because big.matrix objects only store
  // numeric values in R, even if their type is set to 'char'. The types
  // simply correspond to the number of bytes used for each element.
  switch(xpMat->matrix_type()) {
  // case 1:
  //   return subsetBM(xpMat, MatrixAccessor<char>(*xpMat), idxParsed, STRSXP);
  // case 4:
  //   return subsetBM(xpMat, MatrixAccessor<int>(*xpMat), idxParsed, INTSXP);
  case 8:
    res = subsetBM_double(xpMat, MatrixAccessor<double>(*xpMat), idxParsed);
    break;
  default:
    print(wrap(xpMat->matrix_type()));
    // This case should never be encountered unless the implementation of
    // big.matrix changes, but is necessary to implement shut up compiler
    // warnings.
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
  
  reshapeOrDrop(res, reshape, drop);
  return res;
}

/*** R
bigm <- bigmemory::attach.resource(file.path('~/Desktop/junk/', 'bigmemory-ieeg.testfile.desc'))
lazy <- lazyarray::lazyarray('~/Desktop/lazyarray_data/')

lazyarray:::parseAndScheduleBlocks(list(1,1,c(1,NA),c(1,NA)), dim(lazy))

lazy[1:5,1,1:2,1,drop=T]

# Call the Rcpp function.
a <- function(...){
  lazyarray:::subsetBM(bigm@address, environment(), dim(lazy)) 
}
res <- a(1:5,1,1:2,1:2)
res - lazy[1:5,1,1:2,1:2,drop=F]
identical(a(c(2,NA),c(2,NA),c(2,NA),c(2,NA)), lazy[c(2,NA),c(2,NA),c(2,NA),c(2,NA),drop=F])
sa <- sample(84, 20, replace = TRUE); sa[sample(20, 3)] <- NA
identical(a(sa,,sa,), lazy[sa,,sa,,drop=F])

print(res)
*/
