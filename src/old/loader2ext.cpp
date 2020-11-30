#include "loader2ext.h"

#include "common.h"
#include "utils.h"
#include "indexConvert.h"
#include "fstWrapper.h"
#include "openMPInterface.h"
using namespace Rcpp;


template <SEXPTYPE RTYPE>
SEXP subsetFSTtemplate(const std::string& rootPath, const std::vector<int64_t>& dim, const List& subparsed){
  const int subset_mode = subparsed["subset_mode"];
  const std::vector<int64_t> target_dimension = subparsed["target_dimension"];
  const int64_t expected_length = subparsed["expected_length"];
  const LogicalVector negative_subscript = subparsed["negative_subscript"];
  List location_indices = subparsed["location_indices"];
  
  StringVector cnames; 
  bool is_complex = RTYPE == CPLXSXP;
  if(is_complex){
    cnames = StringVector::create("V1R", "V1I");
  } else {
    cnames = StringVector::create("V1");
  }
  
  // create results
  Vector<RTYPE> res = static_cast<Vector<RTYPE>>(no_init(expected_length));
  auto *ptr_res = res.begin();
  auto *ptr_alt = ptr_res;
  
  // for blocked runs (if needed)
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  // temporary variables
  List tmp;
  Vector<RTYPE> buffer = Vector<RTYPE>(0);
  auto ptr_buffer = buffer.begin();
  auto na_value = Vector<RTYPE>::get_na();
  
  if(subset_mode == LASUBMOD_NOIDX){
    // case: subset_mode == 2, x[]
    
    ptr_res = res.begin();
    
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){
      partition_path = rootPath + std::to_string(file_ii) + ".fst";
      R_CheckUserInterrupt();
      
      if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
        // this file is invalid, fill with na
        ptr_alt = ptr_res + block_size;
        while( ptr_alt != ptr_res ){
          *ptr_res++ = na_value;
        }
        
      } else {
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        
        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != block_size){
            buffer = static_cast<Vector<RTYPE>>(no_init(block_size));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1R" ]), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1I" ]), false);
        } else {
          buffer = as<Vector<RTYPE>>(tmp["V1"]);
        }
        
        // copy to ptr_res. Unfortunately, we can't use memcpy
        // however, pointers are also fast
        // std::memcpy(ptr_res, INTEGER(buffer), block_size * sizeof(int));
        ptr_buffer = buffer.begin();
        ptr_alt = ptr_res + block_size;
        while(ptr_res != ptr_alt){
          *ptr_res++ = *ptr_buffer++;
        }
      }
      
    }
    
    res.attr("dim") = wrap(target_dimension);
  } else if(subset_mode == LASUBMOD_SINGLE){
    // case: subset_mode == 1, x[i], and i can't be R missing value
#ifndef RCPP_WARN_ON_COERCE
#define RCPP_WARN_ON_COERCE
    NumericVector indices = as<NumericVector>(location_indices[0]);
#undef RCPP_WARN_ON_COERCE
#endif
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
        
        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != reader_end - reader_start + 1){
            buffer = static_cast<Vector<RTYPE>>(no_init(reader_end - reader_start + 1));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1R" ]), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1I" ]), false);
        } else {
          buffer = as<Vector<RTYPE>>(tmp["V1"]);
        }
        
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
            *ptr_res = *(buffer.begin() + (subidx - chunk_start - reader_start));
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
    
    
  } else if (subset_mode == LASUBMOD_MULTI) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    
    // get parsed schedules
    // dim = [block dim, schedule dim, partition counts]
    // [5,16] => block_ndims=0, [5] x [1,1] x [16]
    // [3000,7,3] if block_ndims=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] block_ndims=2 => [100,100] x [100,100,1] x [1]
    
    const List schedule = subparsed["schedule"];
    
    // partition level
    // int64_t partition_counts = schedule["partition_counts"];               // the last dimension - n files to iterate through
    std::vector<int64_t> partition_index = schedule["partition_index"];    // detailed indexes   - always exists
    
    // schedule level
    // int64_t schedule_counts_per_part = schedule["schedule_counts_per_part"]; // for each partition, number of blocks to run
    std::vector<int64_t> schedule_index = schedule["schedule_index"];      // indices to schedule run blocks
    std::vector<int64_t> schedule_dimension = schedule["schedule_dimension"]; // [schedule dim, partition counts]
    
    // block level
    int64_t block_ndims = schedule["block_ndims"];                    // length(block dim)
    std::vector<int64_t> block_dimension = schedule["block_dimension"]; // [block dim], full version
    std::vector<int64_t> block_prod_dim = schedule["block_prod_dim"]; // prod([1, block dim]), used to locate indices when block is too large to index
    std::vector<int64_t> block_schedule = schedule["block_schedule"]; // given a flattened block (full version), which indices to subset?
    int64_t block_schedule_start = schedule["block_schedule_start"];
    int64_t block_schedule_end = schedule["block_schedule_end"];      // min, max of block_schedule
    
    int64_t block_length = schedule["block_length"];                  // # elements in a block (full version) = prod(block_dimension)
    int64_t block_expected_length = schedule["block_expected_length"];// # elements in a block (subset version) = length(block_schedule)
    
    bool block_indexed = schedule["block_indexed"];                   // whether schedule_index can be trusted
    const List block_location = schedule["block_location"];           // subset of locational indices of blocks
    
    // block_location will be used, make int64_t version
    std::vector<std::vector<int64_t>> block_location_alt(block_location.size());
    if(!block_indexed){
      for(int64_t ii = 0; ii < block_location.size(); ii++ ){
        if(block_location[ii] != R_MissingArg){
          block_location_alt[ii] = as<std::vector<int64_t>>(block_location[ii]);
        } else {
          block_location_alt[ii] = std::vector<int64_t>(0);
        }
      }
    }
    
    ptr_res = INTEGER(res);
    for(int64_t li = 0; li < partition_index.size(); li++){
      
      R_CheckUserInterrupt();
      
      int64_t lidx = partition_index[li];
      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64 || lidx == NA_REAL){
        // name is messed up here,
        // block_size = block_expected_length * schedule_counts_per_part
        ptr_alt = ptr_res + block_size;
        for(;ptr_res != ptr_alt; ptr_res++){
          *ptr_res = na_value;
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
          *ptr_res = na_value;
        }
        continue;
      }
      
      // ptr_res += block_size;
      // recursively read in block_size of data
      
      chunk_start = 0;
      chunk_end = block_length;
      
      for(std::vector<int64_t>::iterator ptr_block = schedule_index.begin(); 
          ptr_block != schedule_index.end(); 
          ptr_block ++ ){
        
        // print(wrap(*ptr_block));
        
        if(*ptr_block == NA_INTEGER64 || !(block_schedule_start > 0 && block_schedule_start <= block_schedule_end)){
          // fill NAs
          ptr_alt = ptr_res + block_expected_length; // block length (subset version)
          for(;ptr_res != ptr_alt; ptr_res++){
            *ptr_res = na_value;
          }
          continue;
        }
        
        // locate where the rows are in the fst file
        chunk_end = block_length * (*ptr_block);
        chunk_start = chunk_end - block_length;
        // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
        reader_start = (int)(chunk_start + block_schedule_start);
        reader_end = (int)(chunk_start + block_schedule_end);
        tmp = fstRetrieve(partition_path, wrap(cnames), wrap(reader_start), wrap(reader_end));
        tmp = tmp["resTable"];
        
        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != reader_end - reader_start + 1){
            buffer = static_cast<Vector<RTYPE>>(no_init(reader_end - reader_start + 1));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1R" ]), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(tmp[ "V1I" ]), false);
        } else {
          buffer = as<Vector<RTYPE>>(tmp["V1"]);
        }
        ptr_buffer = buffer.begin();
        
        // dynamically allocate arrays to persist indices
        int nThread = getLazyThread();
        if(nThread <= 1){ nThread = 1; }
        
        if(!block_indexed){
          // non-indexed (usually memory too big for index), index on the fly
          
          // openmp-able indexing
          // 800MB single index took 2+sec now 1.3s
#pragma omp parallel num_threads(nThread)
{
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < block_expected_length; ii++ ){
    // int current_thread = ii % nThread;
    // std::vector<int64_t> index = index_buffer[current_thread];
    
    int64_t mod;
    int64_t rest = ii;
    int64_t sub_index = 0;
    int64_t subblock_dim_ii;
    int64_t tmp;
    // print(wrap(ii));
    // Rcout << subblock_dim[0] << " "<< subblock_dim[1] << " "<< subblock_dim[2] << " \n";
    for(int64_t di = 0; di < block_ndims; di++ ){
      
      // block_dimension = schedule["block_dimension"]; // [block dim], full version
      // std::vector<int64_t> block_prod_dim = schedule["block_prod_dim"]; // prod([1, block dim]), used to locate indices when block is too large to index
      // std::vector<int64_t> block_schedule = schedule["block_schedule"]; // given a flattened block (full version), which indices to subset?
      // int64_t block_schedule_start = schedule["block_schedule_start"];
      // int64_t block_schedule_end = schedule["block_schedule_end"];      // min, max of block_schedule
      // 
      // int64_t block_length = schedule["block_length"];                  // # elements in a block (full version) = prod(block_dimension)
      // int64_t block_expected_length = schedule["block_expected_length"];// # elements in a block (subset version) = length(block_schedule)
      // 
      // bool block_indexed = schedule["block_indexed"];                   // whether schedule_index can be trusted
      // const List block_location 
      
      if(sub_index != NA_INTEGER64 && sub_index != NA_REAL){
        subblock_dim_ii = *(target_dimension.begin() + di);
        mod = rest % subblock_dim_ii;
        rest = (rest - mod) / subblock_dim_ii;
        
        // Rcout << mod<< " ";
        // get di^th margin element mod
        // partition_subblocklocs[di][mod]
        const std::vector<int64_t>& location_ii = block_location_alt[di];
        if(location_ii.size() == 0){
          // index[di]
          tmp = mod;
        } else {
          // index[di]
          // location_ii starts from 1 but we need it to starting from 0
          tmp = *(location_ii.begin() + mod) - 1;
        }
        
        // is tmp is < 0, that mean it's invalid may be remove the other one
        if(tmp < 0 || tmp == NA_REAL ){
          sub_index = NA_INTEGER64;
        } else if (tmp != NA_INTEGER64){
          sub_index += *(block_prod_dim.begin() + di) * tmp;
        }
      }
      
      
    }
    
    if( sub_index == NA_INTEGER64 ) {
      *(ptr_res + ii) = na_value;
    } else {
      sub_index = sub_index + 1 - block_schedule_start;
      *(ptr_res + ii) = *(ptr_buffer + sub_index);
    }
    
  }
}


// end parallel
ptr_res += block_expected_length;

        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
#pragma omp parallel num_threads(nThread)
{
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < block_expected_length; ii++ ){
    int64_t shift = *(block_schedule.begin() + ii);
    if(shift < block_schedule_start || shift == NA_REAL || shift == NA_INTEGER64){
      *(ptr_res + ii) = na_value;
    } else {
      *(ptr_res + ii) = *(ptr_buffer + (shift - block_schedule_start));
    }
    
  }
} // end parallel

ptr_res += block_expected_length;
        }
        
      }
      // int64_t chunk_start, chunk_end;
      // int reader_start, reader_end;
      // 
    }
    
    res.attr("dim") = wrap(target_dimension);
    
  } else {
    stop("Unknown subset method");
  }
  
  
  return res;
}
