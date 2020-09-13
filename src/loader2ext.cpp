#include "loader2ext.h"

#include "common.h"
#include "utils.h"
#include "indexConvert.h"
#include "fstWrapper.h"
#include "openMPInterface.h"
using namespace Rcpp;

SEXP subsetFST_double(const std::string& rootPath, const NumericVector& dim, const List& subparsed){
  Rcpp::Timer _rcpp_timer;
  
  const int subset_mode = subparsed["subset_mode"];
  const NumericVector target_dimension = subparsed["target_dimension"];
  const int64_t expected_length = subparsed["expected_length"];
  const LogicalVector negative_subscript = subparsed["negative_subscript"];
  List location_indices = subparsed["location_indices"];
  
  StringVector cnames = StringVector::create("V1"); 
  SEXP res = PROTECT(Rf_allocVector(REALSXP, expected_length));
  double *ptr_res = REAL(res);
  double *ptr_alt = ptr_res;
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  List tmp;
  CharacterVector colNames;
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  if(subset_mode == 2){
    // case: subset_mode == 2, x[]
    
    tok("S subsetFST_double - mode 2");
    
    ptr_res = REAL(res);
    
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){
      partition_path = rootPath + std::to_string(file_ii) + ".fst";
      R_CheckUserInterrupt();
      if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
        // this file is invalid, fill with na
        ptr_alt = ptr_res + block_size;
        while( ptr_alt != ptr_res ){
          *ptr_res++ = NA_REAL;
        }
        
      } else {
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        SEXP buffer = tmp["V1"];
        
        // check if we can memcpy
        int n_protected = 0;
        if(TYPEOF(buffer) != REALSXP){
          buffer = PROTECT(Rf_coerceVector(buffer, REALSXP));
          n_protected++;
        }
        
        std::memcpy(ptr_res, REAL(buffer), block_size * sizeof(double));
        
        ptr_res += block_size;
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
        
      }
      
    }
    
    
    Rf_setAttrib(res, wrap("dim"), wrap(target_dimension));
    
    tok("E subsetFST_double - mode 2");
  } else if(subset_mode == 1){
    
    tok("S subsetFST_double - mode 1");
    
    // case: subset_mode == 1, x[i], and i can't be R missing value
    NumericVector indices = as<NumericVector>(location_indices[0]);
    bool is_negative = negative_subscript[0];
    
    
    // idx from chunk_start to expect_nrows-1
    chunk_start = 0;
    chunk_end = expect_nrows;
    LogicalVector sel;
    int64_t bump_start = 0;
    bool enable_bump = true;
    
    // TODO: check negative cases
    if(is_negative){
      
      // There is simply no point to do so, data is so large and I'll defer the implementation
      stop("Negative subscript has not been implemented yet.");
      // // No NAs in this case, just iterate through all 
      // // indices is in decending order, start from the end
      // for(StringVector::iterator ptr_file = files.begin(); ptr_file != files.end(); 
      //     ptr_file++, chunk_start += expect_nrows, chunk_end += expect_nrows ) {
      //   
      // }
      
    } else {
      
      // initialize with NA
      ptr_res = REAL(res);
      ptr_alt = ptr_res;
      ptr_res += expected_length;
      for(;ptr_alt != ptr_res; ptr_alt++){
        *ptr_alt = NA_REAL;
      }
      ptr_res = REAL(res);
      
      for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows  ){
        partition_path = rootPath + std::to_string(file_ii) + ".fst";
        R_CheckUserInterrupt();
        
        
        sel = !(is_na(indices) | indices <= chunk_start | indices > chunk_end);
        // get sub index for this chunk, note no NA is included
        NumericVector sub_idx = indices[sel];
        
        // if this chunk is not used, just skip;
        if( sub_idx.size() == 0 ){ continue; }
        
        
        reader_start = (int)(min(sub_idx) - chunk_start);
        reader_end = (int)(max(sub_idx) - chunk_start);
        
        // Rcout << std::to_string(reader_start) << "\n";
        
        // read to buffer
        if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
          // this file is invalid, fill with na, but they have been set to NAs
          continue;
        }
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        const NumericVector& buffer_vec(tmp["V1"]);
        
        ptr_res = REAL(res);
        ptr_alt = ptr_res + expected_length;
        // Skip first bump_start elements as they are all set
        ptr_res = ptr_res + bump_start;
        NumericVector::iterator ptr_sub_idx = sub_idx.begin();
        LogicalVector::iterator ptr_sel = sel.begin() + bump_start;
        NumericVector::iterator ptr_indices = indices.begin()+ bump_start;
        enable_bump = true;
        
        while(ptr_sel != sel.end() || ptr_sub_idx != sub_idx.end() || ptr_res != ptr_alt){
          
          if(*ptr_sel){
            const int64_t subidx = (*ptr_sub_idx);
            *ptr_res = *(buffer_vec.begin() + (subidx - chunk_start - reader_start));
            ptr_sub_idx++;
            *ptr_indices = NA_REAL;
            if( enable_bump ){
              bump_start++;
            }
          } else if(*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
            bump_start++;
          } else {
            enable_bump = false;
          }
          ptr_sel++;
          ptr_res++;
          ptr_indices++;
        }
        
      }
      
    }
    
    tok("E subsetFST_double - mode 1");
  } else if (subset_mode == 0) {
    tok("S subsetFST_double - mode 0");
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    // get partition dim
    R_xlen_t ndims = dim.size();
    NumericVector part_dim = NumericVector(dim.begin(), dim.end());
    *(part_dim.end() - 1) = 1;
    
    // Decide buffer size. if part_dim = c(3000,7,3), then buffer_margin = 1 because part[3000,7,1] has length > BLOCKSIZE, 
    // which is enough to read from 
    int64_t part_block_size = 1;
    R_xlen_t buffer_margin = 0;
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(part_block_size > getLazyBlockSize()){
        break;
      }
      part_block_size *= part_dim[buffer_margin];
    }
    int64_t nblocks = 1;
    
    SEXP lastidx = location_indices[ndims - 1];
    NumericVector last_indices;
    if(lastidx == R_MissingArg){
      last_indices = seq(1, *(dim.end()-1));
    } else {
      last_indices = NumericVector(lastidx);
    }
    
    // USE R internals to handle objects
    R_xlen_t subblock_ndim = buffer_margin > 0 ? buffer_margin : 1;
    R_xlen_t block_ndim = ndims - subblock_ndim;
    if( block_ndim < 2){ block_ndim = 2; }
    
    SEXP partition_subblocklocs = PROTECT(Rf_allocVector( VECSXP, subblock_ndim ));
    std::vector<int64_t> partition_subblockdim( subblock_ndim, 1 );
    std::vector<int64_t> partition_subblockdim_cumprod( subblock_ndim, 1 );
    
    SEXP partition_blocklocs = PROTECT(Rf_allocVector( VECSXP, block_ndim ));
    std::vector<int64_t> partition_blockdim( block_ndim, 1 );
    
    SEXP one = PROTECT(Rf_allocVector(REALSXP, 1));
    REAL(one)[0] = 1;
    
    // 
    // [3000,7,3] if buffer_margin=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] buffer_margin=2 => [100,100] x [100,100,1] x [1]
    
    for( R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      partition_subblockdim[dd] = part_dim[dd];
      SET_VECTOR_ELT(partition_subblocklocs, dd, location_indices[dd]);
      if( dd == 0 ){
        partition_subblockdim_cumprod[0] = 1;
      } else {
        partition_subblockdim_cumprod[dd] = partition_subblockdim_cumprod[dd - 1] * part_dim[dd - 1];
      }
    }
    
    for(R_xlen_t dd = 0; dd < block_ndim; dd++){
      if(dd + subblock_ndim < ndims - 1){
        partition_blockdim[dd] = part_dim[dd + subblock_ndim];
        SET_VECTOR_ELT(partition_blocklocs, dd, location_indices[dd + subblock_ndim]);
      } else {
        partition_blockdim[dd] = 1;
        SET_VECTOR_ELT(partition_blocklocs, dd, one);
      }
    }
    UNPROTECT(1);
    
    /*
     * Optimization here:
     * In the previous implementation, I used
     * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
     * to generate indices. However, for long matrix/arrays, such as matrix with c(2^30, 5) dimension, this could be
     * very slow because a large index set will be generated. 
     * What we want is just to calculate index min/max in the sub-block, hence no need to call loc2idx3
     */
    int64_t subblock_min = 1, subblock_max = 1, subblock_len = 1;
    std::vector<int64_t> subblock_dim = std::vector<int64_t>(subblock_ndim);
    // std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    // 
    // for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++ ){
    //   if(*ptr_subblock_idx != NA_REAL && *ptr_subblock_idx != NA_INTEGER64){
    //     if(subblock_min < 0 || *ptr_subblock_idx < subblock_min){
    //       subblock_min = *ptr_subblock_idx;
    //     }
    //     if(subblock_max < *ptr_subblock_idx){
    //       subblock_max = *ptr_subblock_idx;
    //     }
    //   }
    // }
    
    // variables used: part_dim, partition_subblocklocs
    int64_t mfactor = 1;
    for(R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      SEXP location_ii = VECTOR_ELT(partition_subblocklocs, dd);
      if(location_ii == R_MissingArg){
        // subblock_min += 0
        subblock_max += mfactor * (-1 + part_dim[dd]);
        subblock_len *= part_dim[dd];
        subblock_dim[dd] = part_dim[dd];
      } else {
        NumericVector loc_ii = na_omit( as<NumericVector>(location_ii) );
        subblock_min += mfactor * (min( loc_ii ) - 1);
        subblock_max += mfactor * (max( loc_ii ) - 1);
        subblock_dim[dd] = Rf_xlength(location_ii);
        // print(wrap(subblock_dim[dd]));
        subblock_len *= subblock_dim[dd];
      }
      mfactor *= part_dim[dd];
    }
    
    // If part block size is too large (happends when first several dimensions are too large)
    // don't calculate index set as it takes time
    std::vector<int64_t> subblock_idx = std::vector<int64_t>(0);
    if(subblock_ndim >= 2 && part_block_size < BLOCKLARGE){
      subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    }
    
    std::vector<int64_t> block_idx = loc2idx3(partition_blocklocs, partition_blockdim);
    nblocks = block_idx.size();
    
    // Rcout << std::to_string(buffer_margin) << "\n";
    // Rcout << std::to_string(part_block_size) << "\n";
    // Rcout << std::to_string(nblocks) << "\n";
    
    ptr_res = REAL(res);
    for(R_xlen_t li = 0; li < last_indices.size(); li++){
      R_CheckUserInterrupt();
      
      int64_t lidx = last_indices[li];
      
      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64){
        
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_REAL;
        }
        continue;
      }
      // Check file valid?
      partition_path = rootPath + std::to_string(lidx) + ".fst";
      
      // check if the file is valid
      if(!checkFstMeta(partition_path, expect_nrows, cnames)){
        
        // print(wrap(file));
        // file is missing or broken, 
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_REAL;
        }
        continue;
      }
      
      // ptr_res += block_size;
      // recursively read in block_size of data
      
      chunk_start = 0;
      chunk_end = part_block_size;
      
      for(std::vector<int64_t>::iterator ptr_block = block_idx.begin(); 
          ptr_block != block_idx.end(); 
          ptr_block ++ ){
        
        // print(wrap(*ptr_block));
        
        if(*ptr_block == NA_INTEGER64 || !(subblock_min > 0 && subblock_min <= subblock_max)){
          // fill NAs
          ptr_alt = ptr_res + subblock_len; // subblock_idx.size();
          for(;ptr_res != ptr_alt; ptr_res++){
            *ptr_res = NA_REAL;
          }
          continue;
        }
        
        chunk_start = part_block_size * (*ptr_block) - part_block_size;
        chunk_end = chunk_start + part_block_size;
        // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
        reader_start = (int)(chunk_start + subblock_min);
        reader_end = (int)(chunk_start + subblock_max);
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        
        SEXP buffer = tmp["V1"];
        int n_protected = 0;
        if(TYPEOF(buffer) != REALSXP){
          buffer = PROTECT(Rf_coerceVector(buffer, REALSXP));
          n_protected++;
        }
        
        /*
         * iterate through locations
         * original definition
         * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
         * partition_subblocklocs is vecsxp
         * partition_subblockdim is std::vector<int64_t> - subset of dim
         * subblock_dim is std::vector<int64_t> - dimension of subblock dimension
         * subblock_len is int64_t, prod of subblock_dim, subblock_idx.size()
         */

        
        // subblock_dim = [3,4,5] iterate from 0 to 59
        
        // dynamically allocate arrays to persist indices
        int nThread = getLazyThread();
        if(nThread <= 1){
          nThread = 1;
        }
        
        if(subblock_idx.size() != subblock_len){
          
        // openmp-able indexing
        // 800MB single index took 2+sec now 1.3s
#pragma omp parallel num_threads(nThread)
          {
#pragma omp for schedule(static, 1) nowait
            for(int64_t ii = 0; ii < subblock_len; ii++ ){
              // int current_thread = ii % nThread;
              // std::vector<int64_t> index = index_buffer[current_thread];
              
              int64_t mod;
              int64_t rest = ii;
              int64_t sub_index = 0;
              int64_t subblock_dim_ii;
              int64_t tmp;
              // print(wrap(ii));
              // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
              for(int64_t di = 0; di < subblock_ndim; di++ ){
                
                if(sub_index != NA_INTEGER64){
                  subblock_dim_ii = *(subblock_dim.begin() + di);
                  mod = rest % subblock_dim_ii;
                  rest = (rest - mod) / subblock_dim_ii;
                  
                  // Rcout << mod<< " ";
                  // get di^th margin element mod
                  // partition_subblocklocs[di][mod]
                  SEXP location_ii = VECTOR_ELT(partition_subblocklocs, di);
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
                    sub_index += *(partition_subblockdim_cumprod.begin() + di) * tmp;
                  }
                }
                
                
              }
              // Rcout << "\n";
              // 
              // print(wrap(ii));
              // print(wrap(sub_index));
              // Rcout << partition_subblockdim_cumprod[0] << " "<< partition_subblockdim_cumprod[1] << " "<< partition_subblockdim_cumprod[2] << " \n";
              // Rcout<<"\n";
              
              // locate ii in re
              // *(ptr_res + ii) = xxx
              
              // for c(2,3,4), ii=11-1 => index=[0,2,1]
              // sub_index converts back to index within sub-block
              // in this example, partition_subblockdim_cumprod = c(1,4,20) if dim = c(4,5,6)
              // sub_index = 0 + 2 * 4 + 1 * 20 = 28
              
              // get sub_index^th actual element from buffer
              // buffer starts from subblock_min, hence buffer[sub_index + 1 - subblock_min]
              
              if( sub_index == NA_INTEGER64 ) {
                *(ptr_res + ii) = NA_REAL;
              } else {
                sub_index = sub_index + 1 - subblock_min;
                *(ptr_res + ii) = *(REAL(buffer) + sub_index);
              }
                
            }
          } // end parallel
          
          ptr_res += subblock_len;
        
        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
          double *buffer_start = REAL(buffer);
#pragma omp parallel num_threads(nThread)
          {
#pragma omp for schedule(static, 1) nowait
            for(int64_t ii = 0; ii < subblock_len; ii++ ){
              int64_t shift = *(subblock_idx.begin() + ii);
              if(shift == NA_REAL || shift == NA_INTEGER64){
                *(ptr_res + ii) = NA_REAL;
              } else {
                *(ptr_res + ii) = *(buffer_start + (shift - subblock_min));
              }
              
            }
          } // end parallel
          
          ptr_res += subblock_len;

        }
        
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
        
      }
      // int64_t chunk_start, chunk_end;
      // int reader_start, reader_end;
      // 
    }
    
    
    Rf_setAttrib(res, wrap("dim"), target_dimension);
    UNPROTECT(2);
    
    // 
    // // Get max and min for each dim
    // print(location_indices);
    // List part_idx = List(location_indices.begin(), location_indices.end() - 1);
    // IntegerVector part_dim = IntegerVector(dim.begin(), dim.end());
    // List  loc2idx(location_indices, dim){
    
  } else {
    stop("Unknown subset method");
  }
  
  _rcpp_timer.step("finished");
  
  UNPROTECT(1);
  
  // NumericVector _res(_rcpp_timer);
  // _res = _res / 1000000.0;
  // Rcpp::print(_res);
  
  return res;
  
}

SEXP subsetFST_integer(const std::string& rootPath, const NumericVector& dim, const List& subparsed){
  
  const int subset_mode = subparsed["subset_mode"];
  const NumericVector target_dimension = subparsed["target_dimension"];
  const int64_t expected_length = subparsed["expected_length"];
  const LogicalVector negative_subscript = subparsed["negative_subscript"];
  List location_indices = subparsed["location_indices"];
  
  StringVector cnames = StringVector::create("V1"); 
  
  SEXP res = PROTECT(Rf_allocVector(INTSXP, expected_length));
  int *ptr_res = INTEGER(res);
  int *ptr_alt = ptr_res;
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  List tmp;
  CharacterVector colNames;
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  if(subset_mode == 2){
    // case: subset_mode == 2, x[]
    
    ptr_res = INTEGER(res);
    
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){
      partition_path = rootPath + std::to_string(file_ii) + ".fst";
      R_CheckUserInterrupt();
      
      if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
        // this file is invalid, fill with na
        ptr_alt = ptr_res + block_size;
        while( ptr_alt != ptr_res ){
          *ptr_res++ = NA_INTEGER;
        }
        
      } else {
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        SEXP buffer = tmp["V1"];
        
        // check if we can memcpy
        int n_protected = 0;
        if(TYPEOF(buffer) != INTSXP){
          buffer = PROTECT(Rf_coerceVector(buffer, INTSXP));
          n_protected++;
        }
        
        std::memcpy(ptr_res, INTEGER(buffer), block_size * sizeof(int));
        
        ptr_res += block_size;
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
      }
      
    }
    
    Rf_setAttrib(res, wrap("dim"), wrap(target_dimension));
  } else if(subset_mode == 1){
    // case: subset_mode == 1, x[i], and i can't be R missing value
    NumericVector indices = as<NumericVector>(location_indices[0]);
    bool is_negative = negative_subscript[0];
    
    
    // idx from chunk_start to expect_nrows-1
    chunk_start = 0;
    chunk_end = expect_nrows;
    LogicalVector sel;
    int64_t bump_start = 0;
    bool enable_bump = true;
    
    // TODO: check negative cases
    if(is_negative){
      
      // There is simply no point to do so, data is so large and I'll defer the implementation
      stop("Negative subscript has not been implemented yet.");
      // // No NAs in this case, just iterate through all 
      // // indices is in decending order, start from the end
      // for(StringVector::iterator ptr_file = files.begin(); ptr_file != files.end(); 
      //     ptr_file++, chunk_start += expect_nrows, chunk_end += expect_nrows ) {
      //   
      // }
      
    } else {
      
      // initialize with NA
      ptr_res = INTEGER(res);
      ptr_alt = ptr_res;
      ptr_res += expected_length;
      for(;ptr_alt != ptr_res; ptr_alt++){
        *ptr_alt = NA_INTEGER;
      }
      ptr_res = INTEGER(res);
      
      for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows ){
        partition_path = rootPath + std::to_string(file_ii) + ".fst";
        R_CheckUserInterrupt();
        
        sel = !(is_na(indices) | indices <= chunk_start | indices > chunk_end);
        // get sub index for this chunk, note no NA is included
        NumericVector sub_idx = indices[sel];
        
        // if this chunk is not used, just skip;
        if( sub_idx.size() == 0 ){ continue; }
        
        
        reader_start = (int)(min(sub_idx) - chunk_start);
        reader_end = (int)(max(sub_idx) - chunk_start);
        
        // Rcout << std::to_string(reader_start) << "\n";
        
        // read to buffer
        if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
          // this file is invalid, fill with na, but they have been set to NAs
          continue;
        }
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        const IntegerVector& buffer_vec(tmp["V1"]);
        
        ptr_res = INTEGER(res);
        ptr_alt = ptr_res + expected_length;
        // Skip first bump_start elements as they are all set
        ptr_res = ptr_res + bump_start;
        NumericVector::iterator ptr_sub_idx = sub_idx.begin();
        LogicalVector::iterator ptr_sel = sel.begin() + bump_start;
        NumericVector::iterator ptr_indices = indices.begin()+ bump_start;
        enable_bump = true;
        
        while(ptr_sel != sel.end() || ptr_sub_idx != sub_idx.end() || ptr_res != ptr_alt){
          
          if(*ptr_sel){
            const int64_t subidx = (*ptr_sub_idx);
            *ptr_res = *(buffer_vec.begin() + (subidx - chunk_start - reader_start));
            ptr_sub_idx++;
            *ptr_indices = NA_REAL;
            if( enable_bump ){
              bump_start++;
            }
          } else if(*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
            bump_start++;
          } else {
            enable_bump = false;
          }
          ptr_sel++;
          ptr_res++;
          ptr_indices++;
        }
        
      }
      
    }
    
    
  } else if (subset_mode == 0) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    
    // get partition dim
    R_xlen_t ndims = dim.size();
    NumericVector part_dim = NumericVector(dim.begin(), dim.end());
    *(part_dim.end() - 1) = 1;
    
    // Decide buffer size. if part_dim = c(3000,7,3), then buffer_margin = 1 because part[3000,7,1] has length > BLOCKSIZE, 
    // which is enough to read from 
    int64_t part_block_size = 1;
    R_xlen_t buffer_margin = 0;
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(part_block_size > getLazyBlockSize()){
        break;
      }
      part_block_size *= part_dim[buffer_margin];
    }
    int64_t nblocks = 1;
    
    SEXP lastidx = location_indices[ndims - 1];
    NumericVector last_indices;
    if(lastidx == R_MissingArg){
      last_indices = seq(1, *(dim.end()-1));
    } else {
      last_indices = NumericVector(lastidx);
    }
    
    // USE R internals to handle objects
    R_xlen_t subblock_ndim = buffer_margin > 0 ? buffer_margin : 1;
    R_xlen_t block_ndim = ndims - subblock_ndim;
    if( block_ndim < 2){ block_ndim = 2; }
    
    SEXP partition_subblocklocs = PROTECT(Rf_allocVector( VECSXP, subblock_ndim ));
    std::vector<int64_t> partition_subblockdim( subblock_ndim, 1 );
    std::vector<int64_t> partition_subblockdim_cumprod( subblock_ndim, 1 );
    
    SEXP partition_blocklocs = PROTECT(Rf_allocVector( VECSXP, block_ndim ));
    std::vector<int64_t> partition_blockdim( block_ndim, 1 );
    
    SEXP one = PROTECT(Rf_allocVector(REALSXP, 1));
    REAL(one)[0] = 1;
    
    // [5,16] => buffer_margin=0, [5] x [1,1] x [16]
    // [3000,7,3] if buffer_margin=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] buffer_margin=2 => [100,100] x [100,100,1] x [1]
    
    for( R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      partition_subblockdim[dd] = part_dim[dd];
      SET_VECTOR_ELT(partition_subblocklocs, dd, location_indices[dd]);
      if( dd == 0 ){
        partition_subblockdim_cumprod[0] = 1;
      } else {
        partition_subblockdim_cumprod[dd] = partition_subblockdim_cumprod[dd - 1] * part_dim[dd - 1];
      }
    }
    
    for(R_xlen_t dd = 0; dd < block_ndim; dd++){
      if(dd + subblock_ndim < ndims - 1){
        partition_blockdim[dd] = part_dim[dd + subblock_ndim];
        SET_VECTOR_ELT(partition_blocklocs, dd, location_indices[dd + subblock_ndim]);
      } else {
        partition_blockdim[dd] = 1;
        SET_VECTOR_ELT(partition_blocklocs, dd, one);
      }
    }
    UNPROTECT(1);
    
    /*
     * Optimization here:
     * In the previous implementation, I used
     * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
     * to generate indices. However, for long matrix/arrays, such as matrix with c(2^30, 5) dimension, this could be
     * very slow because a large index set will be generated. 
     * What we want is just to calculate index min/max in the sub-block, hence no need to call loc2idx3
     */
    int64_t subblock_min = 1, subblock_max = 1, subblock_len = 1;
    std::vector<int64_t> subblock_dim = std::vector<int64_t>(subblock_ndim);
    // std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    // 
    // for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++ ){
    //   if(*ptr_subblock_idx != NA_REAL && *ptr_subblock_idx != NA_INTEGER64){
    //     if(subblock_min < 0 || *ptr_subblock_idx < subblock_min){
    //       subblock_min = *ptr_subblock_idx;
    //     }
    //     if(subblock_max < *ptr_subblock_idx){
    //       subblock_max = *ptr_subblock_idx;
    //     }
    //   }
    // }
    
    // variables used: part_dim, partition_subblocklocs
    int64_t mfactor = 1;
    for(R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      SEXP location_ii = VECTOR_ELT(partition_subblocklocs, dd);
      if(location_ii == R_MissingArg){
        // subblock_min += 0
        subblock_max += mfactor * (-1 + part_dim[dd]);
        subblock_len *= part_dim[dd];
        subblock_dim[dd] = part_dim[dd];
      } else {
        NumericVector loc_ii = na_omit( as<NumericVector>(location_ii) );
        subblock_min += mfactor * (min( loc_ii ) - 1);
        subblock_max += mfactor * (max( loc_ii ) - 1);
        subblock_dim[dd] = Rf_xlength(location_ii);
        // print(wrap(subblock_dim[dd]));
        subblock_len *= subblock_dim[dd];
      }
      mfactor *= part_dim[dd];
    }
    
    // If part block size is too large (happends when first several dimensions are too large)
    // don't calculate index set as it takes time
    std::vector<int64_t> subblock_idx = std::vector<int64_t>(0);
    if(subblock_ndim >= 2 && part_block_size < BLOCKLARGE){
      subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    }
    
    std::vector<int64_t> block_idx = loc2idx3(partition_blocklocs, partition_blockdim);
    nblocks = block_idx.size();
    
    // Rcout << std::to_string(buffer_margin) << "\n";
    // Rcout << std::to_string(part_block_size) << "\n";
    // Rcout << std::to_string(nblocks) << "\n";
    
    
    ptr_res = INTEGER(res);
    for(R_xlen_t li = 0; li < last_indices.size(); li++){
      
      R_CheckUserInterrupt();
      
      int64_t lidx = last_indices[li];
      
      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64){
        
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_INTEGER;
        }
        continue;
      }
      // Check file valid?
      partition_path = rootPath + std::to_string(lidx) + ".fst";
      
      // check if the file is valid
      if(!checkFstMeta(partition_path, expect_nrows, cnames)){
        
        // print(wrap(file));
        // file is missing or broken, 
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_INTEGER;
        }
        continue;
      }
      
      // ptr_res += block_size;
      // recursively read in block_size of data
      
      chunk_start = 0;
      chunk_end = part_block_size;
      
      for(std::vector<int64_t>::iterator ptr_block = block_idx.begin(); 
          ptr_block != block_idx.end(); 
          ptr_block ++ ){
        
        // print(wrap(*ptr_block));
        
        if(*ptr_block == NA_INTEGER64 || !(subblock_min > 0 && subblock_min <= subblock_max)){
          // fill NAs
          ptr_alt = ptr_res + subblock_len; // subblock_idx.size();
          for(;ptr_res != ptr_alt; ptr_res++){
            *ptr_res = NA_INTEGER;
          }
          continue;
        }
        
        chunk_start = part_block_size * (*ptr_block) - part_block_size;
        chunk_end = chunk_start + part_block_size;
        // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
        reader_start = (int)(chunk_start + subblock_min);
        reader_end = (int)(chunk_start + subblock_max);
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        
        SEXP buffer = tmp["V1"];
        int n_protected = 0;
        if(TYPEOF(buffer) != INTSXP){
          buffer = PROTECT(Rf_coerceVector(buffer, INTSXP));
          n_protected++;
        }
        
        /*
         * iterate through locations
         * original definition
         * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
         * partition_subblocklocs is vecsxp
         * partition_subblockdim is std::vector<int64_t> - subset of dim
         * subblock_dim is std::vector<int64_t> - dimension of subblock dimension
         * subblock_len is int64_t, prod of subblock_dim, subblock_idx.size()
         */
        
        
        // subblock_dim = [3,4,5] iterate from 0 to 59
        
        // dynamically allocate arrays to persist indices
        int nThread = getLazyThread();
        if(nThread <= 1){
          nThread = 1;
        }
        
        if(subblock_idx.size() != subblock_len){
        // openmp-able indexing
        // 800MB single index took 2+sec now 1.3s
#pragma omp parallel num_threads(nThread)
{
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < subblock_len; ii++ ){
    // int current_thread = ii % nThread;
    // std::vector<int64_t> index = index_buffer[current_thread];
    
    int64_t mod;
    int64_t rest = ii;
    int64_t sub_index = 0;
    int64_t subblock_dim_ii;
    int64_t tmp;
    // print(wrap(ii));
    // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
    for(int64_t di = 0; di < subblock_ndim; di++ ){
      
      if(sub_index != NA_INTEGER64){
        subblock_dim_ii = *(subblock_dim.begin() + di);
        mod = rest % subblock_dim_ii;
        rest = (rest - mod) / subblock_dim_ii;
        
        // Rcout << mod<< " ";
        // get di^th margin element mod
        // partition_subblocklocs[di][mod]
        SEXP location_ii = VECTOR_ELT(partition_subblocklocs, di);
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
          sub_index += *(partition_subblockdim_cumprod.begin() + di) * tmp;
        }
      }
      
      
    }
    // Rcout << "\n";
    // 
    // print(wrap(ii));
    // print(wrap(sub_index));
    // Rcout << partition_subblockdim_cumprod[0] << " "<< partition_subblockdim_cumprod[1] << " "<< partition_subblockdim_cumprod[2] << " \n";
    // Rcout<<"\n";
    
    // locate ii in re
    // *(ptr_res + ii) = xxx
    
    // for c(2,3,4), ii=11-1 => index=[0,2,1]
    // sub_index converts back to index within sub-block
    // in this example, partition_subblockdim_cumprod = c(1,4,20) if dim = c(4,5,6)
    // sub_index = 0 + 2 * 4 + 1 * 20 = 28
    
    // get sub_index^th actual element from buffer
    // buffer starts from subblock_min, hence buffer[sub_index + 1 - subblock_min]
    
    if( sub_index == NA_INTEGER64 ) {
      *(ptr_res + ii) = NA_INTEGER;
    } else {
      sub_index = sub_index + 1 - subblock_min;
      *(ptr_res + ii) = *(INTEGER(buffer) + sub_index);
    }
    
  }
}



// end parallel
ptr_res += subblock_len;

        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
          int *ptr_buffer = INTEGER(buffer);
#pragma omp parallel num_threads(nThread)
{
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < subblock_len; ii++ ){
    int64_t shift = *(subblock_idx.begin() + ii);
    if(shift == NA_REAL || shift == NA_INTEGER64){
      *(ptr_res + ii) = NA_INTEGER;
    } else {
      *(ptr_res + ii) = *(ptr_buffer + (shift - subblock_min));
    }
    
  }
} // end parallel

ptr_res += subblock_len;
        }
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
      }
      // int64_t chunk_start, chunk_end;
      // int reader_start, reader_end;
      // 
    }
    
    
    Rf_setAttrib(res, wrap("dim"), target_dimension);
    UNPROTECT(2);
    
    // 
    // // Get max and min for each dim
    // print(location_indices);
    // List part_idx = List(location_indices.begin(), location_indices.end() - 1);
    // IntegerVector part_dim = IntegerVector(dim.begin(), dim.end());
    // List  loc2idx(location_indices, dim){
    
  } else {
    stop("Unknown subset method");
  }
  
  
  
  UNPROTECT(1);
  
  
  
  return res;
  
}

SEXP subsetFST_character(const std::string& rootPath, const NumericVector& dim, const List& subparsed){
  
  const int subset_mode = subparsed["subset_mode"];
  const NumericVector target_dimension = subparsed["target_dimension"];
  const int64_t expected_length = subparsed["expected_length"];
  const LogicalVector negative_subscript = subparsed["negative_subscript"];
  List location_indices = subparsed["location_indices"];
  
  StringVector cnames = StringVector::create("V1"); 
  
  StringVector res = StringVector(expected_length);
  StringVector::iterator ptr_res = res.begin();
  StringVector::iterator ptr_alt = ptr_res;
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  List tmp;
  CharacterVector colNames;
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  if(subset_mode == 2){
    // case: subset_mode == 2, x[]
    
    ptr_res = res.begin();
    
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){
      partition_path = rootPath + std::to_string(file_ii) + ".fst";
      
      R_CheckUserInterrupt();
      
      if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
        // this file is invalid, fill with na
        ptr_alt = ptr_res + block_size;
        std::fill(ptr_res, ptr_alt, NA_STRING);
        ptr_res += block_size;
        
      } else {
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        StringVector buffer = tmp["V1"];
        
        // check if we can memcpy
        std::copy(buffer.begin(), buffer.end(), ptr_res);
        ptr_res += block_size;
        
      }
      
    }
    
    Rf_setAttrib(res, wrap("dim"), wrap(target_dimension));
  } else if(subset_mode == 1){
    // case: subset_mode == 1, x[i], and i can't be R missing value
    NumericVector indices = as<NumericVector>(location_indices[0]);
    bool is_negative = negative_subscript[0];
    
    
    // idx from chunk_start to expect_nrows-1
    chunk_start = 0;
    chunk_end = expect_nrows;
    LogicalVector sel;
    int64_t bump_start = 0;
    bool enable_bump = true;
    
    // TODO: check negative cases
    if(is_negative){
      
      // There is simply no point to do so, data is so large and I'll defer the implementation
      stop("Negative subscript has not been implemented yet.");
      // // No NAs in this case, just iterate through all 
      // // indices is in decending order, start from the end
      // for(StringVector::iterator ptr_file = files.begin(); ptr_file != files.end(); 
      //     ptr_file++, chunk_start += expect_nrows, chunk_end += expect_nrows ) {
      //   
      // }
      
    } else {
      
      // initialize with NA
      std::fill(res.begin(), res.end(), NA_STRING);
      ptr_res = res.begin();
      
      for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows ){
        partition_path = rootPath + std::to_string(file_ii) + ".fst";
        R_CheckUserInterrupt();
        
        sel = !(is_na(indices) | indices <= chunk_start | indices > chunk_end);
        // get sub index for this chunk, note no NA is included
        NumericVector sub_idx = indices[sel];
        
        // if this chunk is not used, just skip;
        if( sub_idx.size() == 0 ){ continue; }
        
        
        reader_start = (int)(min(sub_idx) - chunk_start);
        reader_end = (int)(max(sub_idx) - chunk_start);
        
        // Rcout << std::to_string(reader_start) << "\n";
        
        // read to buffer
        if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
          // this file is invalid, fill with na, but they have been set to NAs
          continue;
        }
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        const StringVector& buffer_vec(tmp["V1"]);
        
        ptr_res = res.begin();
        ptr_alt = ptr_res + expected_length;
        // Skip first bump_start elements as they are all set
        ptr_res = ptr_res + bump_start;
        NumericVector::iterator ptr_sub_idx = sub_idx.begin();
        LogicalVector::iterator ptr_sel = sel.begin() + bump_start;
        NumericVector::iterator ptr_indices = indices.begin()+ bump_start;
        enable_bump = true;
        
        while(ptr_sel != sel.end() || ptr_sub_idx != sub_idx.end() || ptr_res != ptr_alt){
          
          if(*ptr_sel){
            const int64_t subidx = (*ptr_sub_idx);
            *ptr_res = *(buffer_vec.begin() + (subidx - chunk_start - reader_start));
            ptr_sub_idx++;
            *ptr_indices = NA_REAL;
            if( enable_bump ){
              bump_start++;
            }
          } else if(*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
            bump_start++;
          } else {
            enable_bump = false;
          }
          ptr_sel++;
          ptr_res++;
          ptr_indices++;
        }
        
      }
      
    }
    
    
  } else if (subset_mode == 0) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    
    // get partition dim
    R_xlen_t ndims = dim.size();
    NumericVector part_dim = NumericVector(dim.begin(), dim.end());
    *(part_dim.end() - 1) = 1;
    
    // Decide buffer size. if part_dim = c(3000,7,3), then buffer_margin = 1 because part[3000,7,1] has length > BLOCKSIZE, 
    // which is enough to read from 
    int64_t part_block_size = 1;
    R_xlen_t buffer_margin = 0;
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(part_block_size > getLazyBlockSize()){
        break;
      }
      part_block_size *= part_dim[buffer_margin];
    }
    int64_t nblocks = 1;
    
    SEXP lastidx = location_indices[ndims - 1];
    NumericVector last_indices;
    if(lastidx == R_MissingArg){
      last_indices = seq(1, *(dim.end()-1));
    } else {
      last_indices = NumericVector(lastidx);
    }
    
    // USE R internals to handle objects
    R_xlen_t subblock_ndim = buffer_margin > 0 ? buffer_margin : 1;
    R_xlen_t block_ndim = ndims - subblock_ndim;
    if( block_ndim < 2){ block_ndim = 2; }
    
    SEXP partition_subblocklocs = PROTECT(Rf_allocVector( VECSXP, subblock_ndim ));
    std::vector<int64_t> partition_subblockdim( subblock_ndim, 1 );
    std::vector<int64_t> partition_subblockdim_cumprod( subblock_ndim, 1 );
    
    SEXP partition_blocklocs = PROTECT(Rf_allocVector( VECSXP, block_ndim ));
    std::vector<int64_t> partition_blockdim( block_ndim, 1 );
    
    SEXP one = PROTECT(Rf_allocVector(REALSXP, 1));
    REAL(one)[0] = 1;
    
    // 
    // [3000,7,3] if buffer_margin=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] buffer_margin=2 => [100,100] x [100,100,1] x [1]
    
    for( R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      partition_subblockdim[dd] = part_dim[dd];
      SET_VECTOR_ELT(partition_subblocklocs, dd, location_indices[dd]);
      if( dd == 0 ){
        partition_subblockdim_cumprod[0] = 1;
      } else {
        partition_subblockdim_cumprod[dd] = partition_subblockdim_cumprod[dd - 1] * part_dim[dd - 1];
      }
    }
    
    for(R_xlen_t dd = 0; dd < block_ndim; dd++){
      if(dd + subblock_ndim < ndims - 1){
        partition_blockdim[dd] = part_dim[dd + subblock_ndim];
        SET_VECTOR_ELT(partition_blocklocs, dd, location_indices[dd + subblock_ndim]);
      } else {
        partition_blockdim[dd] = 1;
        SET_VECTOR_ELT(partition_blocklocs, dd, one);
      }
    }
    UNPROTECT(1);
    
    /*
     * Optimization here:
     * In the previous implementation, I used
     * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
     * to generate indices. However, for long matrix/arrays, such as matrix with c(2^30, 5) dimension, this could be
     * very slow because a large index set will be generated. 
     * What we want is just to calculate index min/max in the sub-block, hence no need to call loc2idx3
     */
    int64_t subblock_min = 1, subblock_max = 1, subblock_len = 1;
    std::vector<int64_t> subblock_dim = std::vector<int64_t>(subblock_ndim);
    // std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    // 
    // for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++ ){
    //   if(*ptr_subblock_idx != NA_REAL && *ptr_subblock_idx != NA_INTEGER64){
    //     if(subblock_min < 0 || *ptr_subblock_idx < subblock_min){
    //       subblock_min = *ptr_subblock_idx;
    //     }
    //     if(subblock_max < *ptr_subblock_idx){
    //       subblock_max = *ptr_subblock_idx;
    //     }
    //   }
    // }
    
    // variables used: part_dim, partition_subblocklocs
    int64_t mfactor = 1;
    for(R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      SEXP location_ii = VECTOR_ELT(partition_subblocklocs, dd);
      if(location_ii == R_MissingArg){
        // subblock_min += 0
        subblock_max += mfactor * (-1 + part_dim[dd]);
        subblock_len *= part_dim[dd];
        subblock_dim[dd] = part_dim[dd];
      } else {
        NumericVector loc_ii = na_omit( as<NumericVector>(location_ii) );
        subblock_min += mfactor * (min( loc_ii ) - 1);
        subblock_max += mfactor * (max( loc_ii ) - 1);
        subblock_dim[dd] = Rf_xlength(location_ii);
        // print(wrap(subblock_dim[dd]));
        subblock_len *= subblock_dim[dd];
      }
      mfactor *= part_dim[dd];
    }
    
    // If part block size is too large (happends when first several dimensions are too large)
    // don't calculate index set as it takes time
    std::vector<int64_t> subblock_idx = std::vector<int64_t>(0);
    if(subblock_ndim >= 2 && part_block_size < BLOCKLARGE){
      subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    }
    
    std::vector<int64_t> block_idx = loc2idx3(partition_blocklocs, partition_blockdim);
    nblocks = block_idx.size();
    
    // Rcout << std::to_string(buffer_margin) << "\n";
    // Rcout << std::to_string(part_block_size) << "\n";
    // Rcout << std::to_string(nblocks) << "\n";
    
    
    ptr_res = res.begin();
    for(R_xlen_t li = 0; li < last_indices.size(); li++){
      
      R_CheckUserInterrupt();
      
      int64_t lidx = last_indices[li];
      
      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64){
        
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_STRING;
        }
        continue;
      }
      // Check file valid?
      partition_path = rootPath + std::to_string(lidx) + ".fst";
      
      // check if the file is valid
      if(!checkFstMeta(partition_path, expect_nrows, cnames)){
        
        // print(wrap(file));
        // file is missing or broken, 
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = NA_STRING;
        }
        continue;
      }
      
      // ptr_res += block_size;
      // recursively read in block_size of data
      
      chunk_start = 0;
      chunk_end = part_block_size;
      
      for(std::vector<int64_t>::iterator ptr_block = block_idx.begin(); 
          ptr_block != block_idx.end(); 
          ptr_block ++ ){
        
        // print(wrap(*ptr_block));
        
        if(*ptr_block == NA_INTEGER64 || !(subblock_min > 0 && subblock_min <= subblock_max)){
          // fill NAs
          ptr_alt = ptr_res + subblock_len; // subblock_idx.size();
          for(;ptr_res != ptr_alt; ptr_res++){
            *ptr_res = NA_STRING;
          }
          continue;
        }
        
        chunk_start = part_block_size * (*ptr_block) - part_block_size;
        chunk_end = chunk_start + part_block_size;
        // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
        reader_start = (int)(chunk_start + subblock_min);
        reader_end = (int)(chunk_start + subblock_max);
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        
        StringVector buffer = tmp["V1"];
        
        /*
         * iterate through locations
         * original definition
         * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
         * partition_subblocklocs is vecsxp
         * partition_subblockdim is std::vector<int64_t> - subset of dim
         * subblock_dim is std::vector<int64_t> - dimension of subblock dimension
         * subblock_len is int64_t, prod of subblock_dim, subblock_idx.size()
         */
        
        
        // subblock_dim = [3,4,5] iterate from 0 to 59
        
        // dynamically allocate arrays to persist indices
        int nThread = getLazyThread();
        if(nThread <= 1){
          nThread = 1;
        }
        
        if(subblock_idx.size() != subblock_len){
        // openmp-able indexing
        // 800MB single index took 2+sec now 1.3s
          for(int64_t ii = 0; ii < subblock_len; ii++ ){
            // int current_thread = ii % nThread;
            // std::vector<int64_t> index = index_buffer[current_thread];
            
            int64_t mod;
            int64_t rest = ii;
            int64_t sub_index = 0;
            int64_t subblock_dim_ii;
            int64_t tmp;
            // print(wrap(ii));
            // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
            for(int64_t di = 0; di < subblock_ndim; di++ ){
              
              if(sub_index != NA_INTEGER64){
                subblock_dim_ii = *(subblock_dim.begin() + di);
                mod = rest % subblock_dim_ii;
                rest = (rest - mod) / subblock_dim_ii;
                
                // Rcout << mod<< " ";
                // get di^th margin element mod
                // partition_subblocklocs[di][mod]
                SEXP location_ii = VECTOR_ELT(partition_subblocklocs, di);
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
                  sub_index += *(partition_subblockdim_cumprod.begin() + di) * tmp;
                }
              }
              
              
            }
            // Rcout << "\n";
            // 
            // print(wrap(ii));
            // print(wrap(sub_index));
            // Rcout << partition_subblockdim_cumprod[0] << " "<< partition_subblockdim_cumprod[1] << " "<< partition_subblockdim_cumprod[2] << " \n";
            // Rcout<<"\n";
            
            // locate ii in re
            // *(ptr_res + ii) = xxx
            
            // for c(2,3,4), ii=11-1 => index=[0,2,1]
            // sub_index converts back to index within sub-block
            // in this example, partition_subblockdim_cumprod = c(1,4,20) if dim = c(4,5,6)
            // sub_index = 0 + 2 * 4 + 1 * 20 = 28
            
            // get sub_index^th actual element from buffer
            // buffer starts from subblock_min, hence buffer[sub_index + 1 - subblock_min]
            
            if( sub_index == NA_INTEGER64 ) {
              *(ptr_res + ii) = NA_STRING;
            } else {
              sub_index = sub_index + 1 - subblock_min;
              *(ptr_res + ii) = *(buffer.begin() + sub_index);
            }
            
          }
          
          
          
          // end parallel
          ptr_res += subblock_len;
        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
          StringVector::iterator ptr_buffer = buffer.begin();

          for(int64_t ii = 0; ii < subblock_len; ii++ ){
            int64_t shift = *(subblock_idx.begin() + ii);
            if(shift == NA_REAL || shift == NA_INTEGER64){
              *ptr_res++ = NA_STRING;
            } else {
              *ptr_res++ = *(ptr_buffer + (shift - subblock_min));
            }
            
          }

        }

      }
      // int64_t chunk_start, chunk_end;
      // int reader_start, reader_end;
      // 
    }
    
    
    Rf_setAttrib(res, wrap("dim"), target_dimension);
    UNPROTECT(2);
    
    // 
    // // Get max and min for each dim
    // print(location_indices);
    // List part_idx = List(location_indices.begin(), location_indices.end() - 1);
    // IntegerVector part_dim = IntegerVector(dim.begin(), dim.end());
    // List  loc2idx(location_indices, dim){
    
  } else {
    stop("Unknown subset method");
  }
  
  
  
  
  return res;
  
}


SEXP subsetFST_complex(const std::string& rootPath, const NumericVector& dim, const List& subparsed){
  
  const int subset_mode = subparsed["subset_mode"];
  const NumericVector target_dimension = subparsed["target_dimension"];
  const int64_t expected_length = subparsed["expected_length"];
  const LogicalVector negative_subscript = subparsed["negative_subscript"];
  List location_indices = subparsed["location_indices"];
  
  StringVector cnames = StringVector::create("V1R", "V1I"); 
  
  SEXP res = PROTECT(Rf_allocVector(CPLXSXP, expected_length));
  Rcomplex *ptr_res = COMPLEX(res);
  Rcomplex *ptr_alt = ptr_res;
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  List tmp;
  CharacterVector colNames;
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  SEXP buffer_real, buffer_imag;
  double *ptr_buffer_real, *ptr_buffer_imag;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  if(subset_mode == 2){
    // case: subset_mode == 2, x[]
    
    ptr_res = COMPLEX(res);
    
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){
      partition_path = rootPath + std::to_string(file_ii) + ".fst";
      
      R_CheckUserInterrupt();
      
      if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
        // this file is invalid, fill with na
        ptr_alt = ptr_res + block_size;
        while( ptr_alt != ptr_res ){
          (*ptr_res).i = NA_REAL;
          (*ptr_res).r = NA_REAL;
          ptr_res++;
        }
        
      } else {
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        buffer_real = tmp["V1R"];
        buffer_imag = tmp["V1I"];
        
        // check if we can memcpy
        int n_protected = 0;
        if(TYPEOF(buffer_real) != REALSXP){
          buffer_real = PROTECT(Rf_coerceVector(buffer_real, REALSXP));
          n_protected++;
        }
        if(TYPEOF(buffer_imag) != REALSXP){
          buffer_imag = PROTECT(Rf_coerceVector(buffer_imag, REALSXP));
          n_protected++;
        }
        
        ptr_alt = ptr_res + block_size;
        ptr_buffer_real = REAL(buffer_real);
        ptr_buffer_imag = REAL(buffer_imag);
        for(;ptr_res != ptr_alt; ptr_res++, ptr_buffer_real++, ptr_buffer_imag++){
          (*ptr_res).r = *ptr_buffer_real;
          (*ptr_res).i = *ptr_buffer_imag;
        }
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
        
      }
      
    }
    
    Rf_setAttrib(res, wrap("dim"), wrap(target_dimension));
  } else if(subset_mode == 1){
    // case: subset_mode == 1, x[i], and i can't be R missing value
    NumericVector indices = as<NumericVector>(location_indices[0]);
    bool is_negative = negative_subscript[0];
    
    
    // idx from chunk_start to expect_nrows-1
    chunk_start = 0;
    chunk_end = expect_nrows;
    LogicalVector sel;
    int64_t bump_start = 0;
    bool enable_bump = true;
    
    // TODO: check negative cases
    if(is_negative){
      
      // There is simply no point to do so, data is so large and I'll defer the implementation
      stop("Negative subscript has not been implemented yet.");
      // // No NAs in this case, just iterate through all 
      // // indices is in decending order, start from the end
      // for(StringVector::iterator ptr_file = files.begin(); ptr_file != files.end(); 
      //     ptr_file++, chunk_start += expect_nrows, chunk_end += expect_nrows ) {
      //   
      // }
      
    } else {
      
      // initialize with NA
      ptr_res = COMPLEX(res);
      ptr_alt = ptr_res;
      ptr_res += expected_length;
      for(;ptr_alt != ptr_res; ptr_alt++){
        (*ptr_alt).i = NA_REAL;
        (*ptr_alt).r = NA_REAL;
      }
      ptr_res = COMPLEX(res);
      
      for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows ){
        partition_path = rootPath + std::to_string(file_ii) + ".fst";
        R_CheckUserInterrupt();
        
        sel = !(is_na(indices) | indices <= chunk_start | indices > chunk_end);
        // get sub index for this chunk, note no NA is included
        NumericVector sub_idx = indices[sel];
        
        // if this chunk is not used, just skip;
        if( sub_idx.size() == 0 ){ continue; }
        
        
        reader_start = (int)(min(sub_idx) - chunk_start);
        reader_end = (int)(max(sub_idx) - chunk_start);
        
        // Rcout << std::to_string(reader_start) << "\n";
        
        // read to buffer
        if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
          // this file is invalid, fill with na, but they have been set to NAs
          continue;
        }
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        buffer_real = tmp["V1R"];
        buffer_imag = tmp["V1I"];
        int n_protected = 0;
        if(TYPEOF(buffer_real) != REALSXP){
          buffer_real = PROTECT(Rf_coerceVector(buffer_real, REALSXP));
          n_protected++;
        }
        if(TYPEOF(buffer_imag) != REALSXP){
          buffer_imag = PROTECT(Rf_coerceVector(buffer_imag, REALSXP));
          n_protected++;
        }
        
        ptr_res = COMPLEX(res);
        ptr_alt = ptr_res + expected_length;
        // Skip first bump_start elements as they are all set
        ptr_res = ptr_res + bump_start;
        NumericVector::iterator ptr_sub_idx = sub_idx.begin();
        LogicalVector::iterator ptr_sel = sel.begin() + bump_start;
        NumericVector::iterator ptr_indices = indices.begin()+ bump_start;
        enable_bump = true;
        
        while(ptr_sel != sel.end() || ptr_sub_idx != sub_idx.end() || ptr_res != ptr_alt){
          
          if(*ptr_sel){
            const int64_t subidx = (*ptr_sub_idx) - chunk_start - reader_start;
            (*ptr_res).r = REAL(buffer_real)[subidx];
            (*ptr_res).i = REAL(buffer_imag)[subidx];
            ptr_sub_idx++;
            *ptr_indices = NA_REAL;
            if( enable_bump ){
              bump_start++;
            }
          } else if(*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
            bump_start++;
          } else {
            enable_bump = false;
          }
          ptr_sel++;
          ptr_res++;
          ptr_indices++;
        }
        
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }
      }
      
    }
    
    
  } else if (subset_mode == 0) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    
    // get partition dim
    R_xlen_t ndims = dim.size();
    NumericVector part_dim = NumericVector(dim.begin(), dim.end());
    *(part_dim.end() - 1) = 1;
    
    // Decide buffer size. if part_dim = c(3000,7,3), then buffer_margin = 1 because part[3000,7,1] has length > BLOCKSIZE, 
    // which is enough to read from 
    int64_t part_block_size = 1;
    R_xlen_t buffer_margin = 0;
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(part_block_size > getLazyBlockSize()){
        break;
      }
      part_block_size *= part_dim[buffer_margin];
    }
    int64_t nblocks = 1;
    
    SEXP lastidx = location_indices[ndims - 1];
    NumericVector last_indices;
    if(lastidx == R_MissingArg){
      last_indices = seq(1, *(dim.end()-1));
    } else {
      last_indices = NumericVector(lastidx);
    }
    
    // USE R internals to handle objects
    R_xlen_t subblock_ndim = buffer_margin > 0 ? buffer_margin : 1;
    R_xlen_t block_ndim = ndims - subblock_ndim;
    if( block_ndim < 2){ block_ndim = 2; }
    
    SEXP partition_subblocklocs = PROTECT(Rf_allocVector( VECSXP, subblock_ndim ));
    std::vector<int64_t> partition_subblockdim( subblock_ndim, 1 );
    std::vector<int64_t> partition_subblockdim_cumprod( subblock_ndim, 1 );
    
    SEXP partition_blocklocs = PROTECT(Rf_allocVector( VECSXP, block_ndim ));
    std::vector<int64_t> partition_blockdim( block_ndim, 1 );
    
    SEXP one = PROTECT(Rf_allocVector(REALSXP, 1));
    REAL(one)[0] = 1;
    
    // 
    // [3000,7,3] if buffer_margin=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if buffer_margin=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] buffer_margin=2 => [100,100] x [100,100,1] x [1]
    
    for( R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      partition_subblockdim[dd] = part_dim[dd];
      SET_VECTOR_ELT(partition_subblocklocs, dd, location_indices[dd]);
      if( dd == 0 ){
        partition_subblockdim_cumprod[0] = 1;
      } else {
        partition_subblockdim_cumprod[dd] = partition_subblockdim_cumprod[dd - 1] * part_dim[dd - 1];
      }
    }
    
    for(R_xlen_t dd = 0; dd < block_ndim; dd++){
      if(dd + subblock_ndim < ndims - 1){
        partition_blockdim[dd] = part_dim[dd + subblock_ndim];
        SET_VECTOR_ELT(partition_blocklocs, dd, location_indices[dd + subblock_ndim]);
      } else {
        partition_blockdim[dd] = 1;
        SET_VECTOR_ELT(partition_blocklocs, dd, one);
      }
    }
    UNPROTECT(1);
    
    /*
     * Optimization here:
     * In the previous implementation, I used
     * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
     * to generate indices. However, for long matrix/arrays, such as matrix with c(2^30, 5) dimension, this could be
     * very slow because a large index set will be generated. 
     * What we want is just to calculate index min/max in the sub-block, hence no need to call loc2idx3
     */
    int64_t subblock_min = 1, subblock_max = 1, subblock_len = 1;
    std::vector<int64_t> subblock_dim = std::vector<int64_t>(subblock_ndim);
    // std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    // 
    // for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++ ){
    //   if(*ptr_subblock_idx != NA_REAL && *ptr_subblock_idx != NA_INTEGER64){
    //     if(subblock_min < 0 || *ptr_subblock_idx < subblock_min){
    //       subblock_min = *ptr_subblock_idx;
    //     }
    //     if(subblock_max < *ptr_subblock_idx){
    //       subblock_max = *ptr_subblock_idx;
    //     }
    //   }
    // }
    
    // variables used: part_dim, partition_subblocklocs
    int64_t mfactor = 1;
    for(R_xlen_t dd = 0; dd < subblock_ndim; dd++ ){
      SEXP location_ii = VECTOR_ELT(partition_subblocklocs, dd);
      if(location_ii == R_MissingArg){
        // subblock_min += 0
        subblock_max += mfactor * (-1 + part_dim[dd]);
        subblock_len *= part_dim[dd];
        subblock_dim[dd] = part_dim[dd];
      } else {
        NumericVector loc_ii = na_omit( as<NumericVector>(location_ii) );
        subblock_min += mfactor * (min( loc_ii ) - 1);
        subblock_max += mfactor * (max( loc_ii ) - 1);
        subblock_dim[dd] = Rf_xlength(location_ii);
        // print(wrap(subblock_dim[dd]));
        subblock_len *= subblock_dim[dd];
      }
      mfactor *= part_dim[dd];
    }
    
    // If part block size is too large (happends when first several dimensions are too large)
    // don't calculate index set as it takes time
    std::vector<int64_t> subblock_idx = std::vector<int64_t>(0);
    if(subblock_ndim >= 2 && part_block_size < BLOCKLARGE){
      subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
    }
    
    std::vector<int64_t> block_idx = loc2idx3(partition_blocklocs, partition_blockdim);
    nblocks = block_idx.size();
    
    // Rcout << std::to_string(buffer_margin) << "\n";
    // Rcout << std::to_string(part_block_size) << "\n";
    // Rcout << std::to_string(nblocks) << "\n";
    
    
    ptr_res = COMPLEX(res);
    for(R_xlen_t li = 0; li < last_indices.size(); li++){
      
      R_CheckUserInterrupt();
      
      int64_t lidx = last_indices[li];
      
      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64){
        
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          (*ptr_res).i = NA_REAL;
          (*ptr_res).r = NA_REAL;
        }
        continue;
      }
      // Check file valid?
      partition_path = rootPath + std::to_string(lidx) + ".fst";
      
      // check if the file is valid
      if(!checkFstMeta(partition_path, expect_nrows, cnames)){
        
        // print(wrap(file));
        // file is missing or broken, 
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          (*ptr_res).i = NA_REAL;
          (*ptr_res).r = NA_REAL;
        }
        continue;
      }
      
      // ptr_res += block_size;
      // recursively read in block_size of data
      
      chunk_start = 0;
      chunk_end = part_block_size;
      
      for(std::vector<int64_t>::iterator ptr_block = block_idx.begin(); 
          ptr_block != block_idx.end(); 
          ptr_block ++ ){
        
        // print(wrap(*ptr_block));
        
        if(*ptr_block == NA_INTEGER64 || !(subblock_min > 0 && subblock_min <= subblock_max)){
          // fill NAs
          ptr_alt = ptr_res + subblock_len; // subblock_idx.size();
          for(;ptr_res != ptr_alt; ptr_res++){
            (*ptr_res).i = NA_REAL;
            (*ptr_res).r = NA_REAL;
          }
          continue;
        }
        
        chunk_start = part_block_size * (*ptr_block) - part_block_size;
        chunk_end = chunk_start + part_block_size;
        // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
        reader_start = (int)(chunk_start + subblock_min);
        reader_end = (int)(chunk_start + subblock_max);
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        
        buffer_real = tmp["V1R"];
        buffer_imag = tmp["V1I"];
        int n_protected = 0;
        if(TYPEOF(buffer_real) != REALSXP){
          buffer_real = PROTECT(Rf_coerceVector(buffer_real, REALSXP));
          n_protected++;
        }
        if(TYPEOF(buffer_imag) != REALSXP){
          buffer_imag = PROTECT(Rf_coerceVector(buffer_imag, REALSXP));
          n_protected++;
        }
        
        /*
         * iterate through locations
         * original definition
         * std::vector<int64_t> subblock_idx = loc2idx3(partition_subblocklocs, partition_subblockdim);
         * partition_subblocklocs is vecsxp
         * partition_subblockdim is std::vector<int64_t> - subset of dim
         * subblock_dim is std::vector<int64_t> - dimension of subblock dimension
         * subblock_len is int64_t, prod of subblock_dim, subblock_idx.size()
         */
        
        
        // subblock_dim = [3,4,5] iterate from 0 to 59
        
        // dynamically allocate arrays to persist indices
        int nThread = getLazyThread();
        if(nThread <= 1){
          nThread = 1;
        }
        
        if(subblock_idx.size() != subblock_len) {
        // openmp-able indexing
        // 800MB single index took 2+sec now 1.3s
#pragma omp parallel num_threads(nThread)
        {
#pragma omp for schedule(static, 1) nowait
          for(int64_t ii = 0; ii < subblock_len; ii++ ){
            // int current_thread = ii % nThread;
            // std::vector<int64_t> index = index_buffer[current_thread];
            
            int64_t mod;
            int64_t rest = ii;
            int64_t sub_index = 0;
            int64_t subblock_dim_ii;
            int64_t tmp;
            // print(wrap(ii));
            // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
            for(int64_t di = 0; di < subblock_ndim; di++ ){
              
              if(sub_index != NA_INTEGER64){
                subblock_dim_ii = *(subblock_dim.begin() + di);
                mod = rest % subblock_dim_ii;
                rest = (rest - mod) / subblock_dim_ii;
                
                // Rcout << mod<< " ";
                // get di^th margin element mod
                // partition_subblocklocs[di][mod]
                SEXP location_ii = VECTOR_ELT(partition_subblocklocs, di);
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
                  sub_index += *(partition_subblockdim_cumprod.begin() + di) * tmp;
                }
              }
              
              
            }
            // Rcout << "\n";
            // 
            // print(wrap(ii));
            // print(wrap(sub_index));
            // Rcout << partition_subblockdim_cumprod[0] << " "<< partition_subblockdim_cumprod[1] << " "<< partition_subblockdim_cumprod[2] << " \n";
            // Rcout<<"\n";
            
            // locate ii in re
            // *(ptr_res + ii) = xxx
            
            // for c(2,3,4), ii=11-1 => index=[0,2,1]
            // sub_index converts back to index within sub-block
            // in this example, partition_subblockdim_cumprod = c(1,4,20) if dim = c(4,5,6)
            // sub_index = 0 + 2 * 4 + 1 * 20 = 28
            
            // get sub_index^th actual element from buffer
            // buffer starts from subblock_min, hence buffer[sub_index + 1 - subblock_min]
            
            if( sub_index == NA_INTEGER64 ) {
              (*(ptr_res + ii)).i = NA_REAL;
              (*(ptr_res + ii)).r = NA_REAL;
            } else {
              sub_index = sub_index + 1 - subblock_min;
              (*(ptr_res + ii)).r = *(REAL(buffer_real) + sub_index);
              (*(ptr_res + ii)).i = *(REAL(buffer_imag) + sub_index);
            }
            
          }
        }
        // end parallel
        
          ptr_res += subblock_len;
        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
          double *ptr_buffer_real = REAL(buffer_real);
          double *ptr_buffer_imag = REAL(buffer_imag);
#pragma omp parallel num_threads(nThread)
{
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < subblock_len; ii++ ){
    int64_t shift = *(subblock_idx.begin() + ii);
    if(shift == NA_REAL || shift == NA_INTEGER64){
      (ptr_res + ii)->i = NA_REAL;
      (ptr_res + ii)->r = NA_REAL;
    } else {
      (ptr_res + ii)->r = *(ptr_buffer_real + (shift - subblock_min));
      (ptr_res + ii)->i = *(ptr_buffer_imag + (shift - subblock_min));
    }
    
  }
} // end parallel

ptr_res += subblock_len;
        }
        
        if(n_protected > 0){
          UNPROTECT(n_protected);
        }

      }
      // int64_t chunk_start, chunk_end;
      // int reader_start, reader_end;
      // 
    }
    
    
    Rf_setAttrib(res, wrap("dim"), target_dimension);
    UNPROTECT(2);
    
    // 
    // // Get max and min for each dim
    // print(location_indices);
    // List part_idx = List(location_indices.begin(), location_indices.end() - 1);
    // IntegerVector part_dim = IntegerVector(dim.begin(), dim.end());
    // List  loc2idx(location_indices, dim){
    
  } else {
    stop("Unknown subset method");
  }
  
  
  
  UNPROTECT(1);
  
  
  
  return res;
  
}


