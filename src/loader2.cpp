#include "loader2.h"
// This file is too long, split into multiple files

#include "common.h"
#include "utils.h"
#include "indexConvert.h"
#include "fstWrapper.h"
#include "openMPInterface.h"

using namespace Rcpp;

static ParsedIndex* pre_scheduled = nullptr;

template <SEXPTYPE RTYPE>
SEXP subsetFSTtemplate(const std::string& rootPath, const std::vector<int64_t>& dim, 
                       const ParsedIndex* subparsed){
  int nThread = getLazyThread();
  if(nThread <= 1){ nThread = 1; }
  
  int subset_mode = subparsed->subset_mode;
  const std::vector<int64_t> target_dimension = subparsed->target_dimension;
  const int64_t expected_length = subparsed->expected_length;
  const std::vector<bool> negative_subscript = subparsed->negative_subscript;
  std::vector<std::pair<std::vector<int64_t>, bool>> location_indices = subparsed->location_indices;
  
  StringVector cnames; 
  const bool is_complex = RTYPE == CPLXSXP;
  if(is_complex){
    cnames = StringVector::create("V1R", "V1I");
  } else {
    cnames = StringVector::create("V1");
  }
  
  // create results
  Vector<RTYPE> res = static_cast<Vector<RTYPE>>(no_init(expected_length));
  auto ptr_res = res.begin();
  auto ptr_alt = ptr_res;
  
  // for blocked runs (if needed)
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  
  int64_t chunk_start, chunk_end;
  int reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;
  
  // temporary variables
  SEXP tmp;
  Vector<RTYPE> buffer = Vector<RTYPE>(0);
  auto ptr_buffer = buffer.begin();
  auto na_value = Vector<RTYPE>::get_na();
  
  if( subset_mode == LASUBMOD_SINGLE && std::get<1>(location_indices[0]) ){
    subset_mode = LASUBMOD_NOIDX;
  }
  
  
  if(subset_mode == LASUBMOD_NOIDX){
    // case: subset_mode == 2, x[]
    
    tok("S subsetFSTtemplate - LASUBMOD_NOIDX");
    
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
        tmp = fstRetrieve(partition_path, Shield<SEXP>(wrap(cnames)), Shield<SEXP>(wrap(1)), R_NilValue);
        tmp = VECTOR_ELT(tmp, 2);
        
        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != block_size){
            buffer = static_cast<Vector<RTYPE>>(no_init(block_size));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 0)), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 1)), false);
        } else {
          
          SEXP tmp_var = PROTECT(VECTOR_ELT(tmp, 0));
          if(TYPEOF(tmp_var) != RTYPE){
            tmp_var = PROTECT(Rf_coerceVector(tmp_var, RTYPE));
            buffer = as<Vector<RTYPE>>(tmp_var);
            UNPROTECT(1);
          } else {
            buffer = as<Vector<RTYPE>>(tmp_var);
          }
          UNPROTECT(1);
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
    
    res.attr("dim") = Shield<SEXP>(wrap(target_dimension));
    
    tok("E subsetFSTtemplate - LASUBMOD_NOIDX");
    
  } else if(subset_mode == LASUBMOD_SINGLE){
    
    tok("S subsetFSTtemplate - LASUBMOD_SINGLE");
    // case: subset_mode == 1, x[i], and i can't be R missing value
    std::vector<int64_t> indices = std::get<0>(location_indices[0]);
    std::vector<int64_t>::iterator ptr_indices = indices.begin();
    bool is_negative = negative_subscript[0];
    
    // idx from chunk_start to expect_nrows-1
    chunk_start = 0;
    chunk_end = expect_nrows;
    std::vector<bool> sel = std::vector<bool>(indices.size());
    std::vector<bool>::iterator ptr_sel = sel.begin();
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
      ptr_res = res.begin();
      ptr_alt = ptr_res;
      ptr_res += expected_length;
      for(;ptr_alt != ptr_res; ptr_alt++){
        *ptr_alt = na_value;
      }
      ptr_res = res.begin();
      std::vector<int64_t> sub_idx(0);
      for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows ){
        partition_path = rootPath + std::to_string(file_ii) + ".fst";
        R_CheckUserInterrupt();
        
        int64_t sub_count = 0;
        for(ptr_sel = sel.begin(), ptr_indices = indices.begin(); 
            ptr_sel != sel.end(); ptr_sel++, ptr_indices++ ){
          // na is negative so falls in this range
          if(*ptr_indices <= chunk_start || *ptr_indices > chunk_end || *ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
            *ptr_sel = false;
          } else {
            *ptr_sel = true;
            sub_count++;
          }
        }
        
        // if this chunk is not used, just skip;
        if( sub_count == 0 ){ continue; }
        
        // get sub index for this chunk, note no NA is included
        sub_idx.resize(sub_count);
        std::vector<int64_t>::iterator ptr_sub_idx = sub_idx.begin();
        reader_start = -1;
        reader_end = -1;
        
        for(ptr_sel = sel.begin(), ptr_indices = indices.begin();
            ptr_sub_idx != sub_idx.end() && ptr_indices != indices.end() && ptr_sel != sel.end(); 
            ptr_indices++, ptr_sel++){
          if(*ptr_sel){
            *ptr_sub_idx = *ptr_indices;
            if(reader_start > *ptr_indices || reader_start == -1){
              reader_start = (int)(*ptr_indices);
            }
            if(reader_end < *ptr_indices){
              reader_end = (int)(*ptr_indices);
            }
            // Rcout << *ptr_sub_idx << " ";
            ptr_sub_idx++;
          }
        }
        
        reader_start -= chunk_start;
        reader_end -= chunk_start;
        // Rcout << "\n" << std::to_string(reader_start) << " " << std::to_string(reader_end) << "\n";
        
        // read to buffer
        if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
          // this file is invalid, fill with na, but they have been set to NAs
          continue;
        }
        // Read from fst file, abuse name "meta" a little bit
        tmp = fstRetrieve(partition_path, Shield<SEXP>(wrap(cnames)), 
                          Shield<SEXP>(wrap(reader_start)), Shield<SEXP>(wrap(reader_end)));
        tmp = VECTOR_ELT(tmp, 2);
        
        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != reader_end - reader_start + 1){
            buffer = static_cast<Vector<RTYPE>>(no_init(reader_end - reader_start + 1));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 0)), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 1)), false);
        } else {
          
          SEXP tmp_var = VECTOR_ELT(tmp, 0);
          if(TYPEOF(tmp_var) != RTYPE){
            tmp_var = PROTECT(Rf_coerceVector(tmp_var, RTYPE));
            buffer = as<Vector<RTYPE>>(tmp_var);
            UNPROTECT(1);
          } else {
            buffer = as<Vector<RTYPE>>(tmp_var);
          }
          
        }
        
        ptr_res = res.begin();
        ptr_alt = ptr_res + expected_length;
        // Skip first bump_start elements as they are all set
        ptr_res = ptr_res + bump_start;
        ptr_sub_idx = sub_idx.begin();
        ptr_sel = sel.begin() + bump_start;
        ptr_indices = indices.begin()+ bump_start;
        enable_bump = false; // FIXME?
        
        while(ptr_sel != sel.end() && ptr_sub_idx != sub_idx.end() && ptr_res != ptr_alt){
          
          if(*ptr_sel){
            *ptr_res = *(buffer.begin() + (*ptr_sub_idx++ - chunk_start - reader_start));
            *ptr_indices = NA_INTEGER64;
            if( enable_bump ){
              bump_start++;
            }
          } else if(enable_bump && (*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64)){
            bump_start++;
          } else {
            enable_bump = false;
          }
          // Rcout << bump_start << " ";
          ptr_sel++;
          ptr_res++;
          ptr_indices++;
        }
        
      }
      
    }
    tok("E subsetFSTtemplate - LASUBMOD_SINGLE");
    
  } else if (subset_mode == LASUBMOD_MULTI) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    tok("S subsetFSTtemplate - LASUBMOD_MULTI");
    // get parsed schedules
    // dim = [block dim, schedule dim, partition counts]
    // [5,16] => block_ndims=0, [5] x [1,1] x [16]
    // [3000,7,3] if block_ndims=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] block_ndims=2 => [100,100] x [100,100,1] x [1]

    ScheduledIndex* schedule = subparsed->schedule;

    // partition level
    // int64_t partition_counts = schedule["partition_counts"];               // the last dimension - n files to iterate through
    std::vector<int64_t> partition_index = schedule->partition_index;    // detailed indexes   - always exists

    // schedule level
    // int64_t schedule_counts_per_part = schedule["schedule_counts_per_part"]; // for each partition, number of blocks to run
    std::vector<int64_t> schedule_index = schedule->schedule_index;      // indices to schedule run blocks
    std::vector<int64_t> schedule_dimension = schedule->schedule_dimension; // [schedule dim, partition counts]

    // block level
    int64_t block_ndims = schedule->block_ndims;                    // length(block dim)
    std::vector<int64_t> block_dimension = schedule->block_dimension; // [block dim], full version
    std::vector<int64_t> block_prod_dim = schedule->block_prod_dim; // prod([1, block dim]), used to locate indices when block is too large to index
    std::vector<int64_t> block_schedule = schedule->block_schedule; // given a flattened block (full version), which indices to subset?
    int64_t block_schedule_start = schedule->block_schedule_start;
    int64_t block_schedule_end = schedule->block_schedule_end;      // min, max of block_schedule

    int64_t block_length = schedule->block_length;                  // # elements in a block (full version) = prod(block_dimension)
    int64_t block_expected_length = schedule->block_expected_length;// # elements in a block (subset version) = length(block_schedule)

    bool block_indexed = schedule->block_indexed;                   // whether block_schedule can be trusted
    std::vector<std::pair<std::vector<int64_t>, bool>> block_location = schedule->block_location;           // subset of locational indices of blocks

    // // block_location will be used, make int64_t version
    // std::vector<std::vector<int64_t>> block_location_alt(block_location.size());
    // if(!block_indexed){
    //   for(int64_t ii = 0; ii < block_location.size(); ii++ ){
    //     if(block_location[ii] != R_MissingArg){
    //       block_location_alt[ii] = as<std::vector<int64_t>>(block_location[ii]);
    //     } else {
    //       block_location_alt[ii] = std::vector<int64_t>(0);
    //     }
    //   }
    // 
    // }

    ptr_res = res.begin();
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
        tmp = fstRetrieve(partition_path, Shield<SEXP>(wrap(cnames)), Shield<SEXP>(wrap(reader_start)), Shield<SEXP>(wrap(reader_end)));
        tmp = VECTOR_ELT(tmp, 2);

        if(is_complex){
          // try to reuse buffer, but if not reusable, create new one
          if(buffer.size() != reader_end - reader_start + 1){
            buffer = static_cast<Vector<RTYPE>>(no_init(reader_end - reader_start + 1));
          }
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 0)), true);
          setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 1)), false);
        } else {
          SEXP tmp_var = VECTOR_ELT(tmp, 0);
          if(TYPEOF(tmp_var) != RTYPE){
            tmp_var = PROTECT(Rf_coerceVector(tmp_var, RTYPE));
            buffer = as<Vector<RTYPE>>(tmp_var);
            UNPROTECT(1);
          } else {
            buffer = as<Vector<RTYPE>>(tmp_var);
          }
        }
        ptr_buffer = buffer.begin();

        if(!block_indexed){
          // non-indexed (usually memory too big for index), index on the fly

          // openmp-able indexing
          // 800MB single index took 2+sec now 1.3s
          
#ifdef LAZYARRAY_DEBUG
          print(wrap("Assign index - on the fly"));
#endif // LAZYARRAY_DEBUG
          
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
    // print(wrap(buffer));
    // print(wrap(block_prod_dim));
    // print(wrap(target_dimension));
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


        // get di^th margin element mod
        // partition_subblocklocs[di][mod]
        
        // check this lication is empty
        if( std::get<1>(block_location[di]) ){
          // index[di], location_ii is missing
          tmp = mod + 1;
        } else {
          // index[di]
          tmp = std::get<0>(block_location[di])[mod];
        }
        // print(wrap(location_ii));
        // Rcout << di << " " << mod << " " << tmp << " ";
        // is tmp is < 1, that mean it's invalid may be remove the other one
        if(tmp < 1 || tmp == NA_REAL ){
          sub_index = NA_INTEGER64;
        } else if (tmp != NA_INTEGER64){
          // location_ii starts from 1 but we need it to starting from 0
          sub_index += *(block_prod_dim.begin() + di) * (tmp - 1);
        }
      }


    }
    // Rcout << block_schedule_start << " " << sub_index<< "\n";

    if( sub_index == NA_INTEGER64 ) {
      *(ptr_res + ii) = na_value;
    } else {
      sub_index = sub_index + 1 - block_schedule_start;
      *(ptr_res + ii) = *(buffer.begin() + sub_index);
    }
    // Rcout << *(ptr_res + ii) << "\n";
  }
}


// end parallel
ptr_res += block_expected_length;
// print(wrap(partition_path));
//
// print(wrap(block_expected_length));
// print(wrap(buffer));
// print(wrap(res));
        } else {
          // don't calculate index on the fly.
          // subblock_idx
          // subblock_idx.size() == subblock_len
          
#ifdef LAZYARRAY_DEBUG
          print(wrap("Assign index - pre-scheduled"));
#endif // LAZYARRAY_DEBUG
          
#pragma omp parallel num_threads(nThread) 
{
  int64_t shift;
#pragma omp for schedule(static, 1) nowait
  for(int64_t ii = 0; ii < block_expected_length; ii++ ){
    shift = *(block_schedule.begin() + ii);
    auto ptr_res2 = ptr_res + ii;
    if(shift < block_schedule_start || shift == NA_REAL || shift == NA_INTEGER64){
      *ptr_res2 = na_value;
    } else {
      *ptr_res2 = *(ptr_buffer + (shift - block_schedule_start));
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

    tok("E subsetFSTtemplate - LASUBMOD_MULTI");

  } else {
    stop("Unknown subset method");
  }
  
  
  return res;
}


SEXP subsetFSTBare(const std::string& rootPath, const ParsedIndex* parsed,
                   const std::vector<int64_t>& dim, const SEXPTYPE& dtype) {
  tok("S subsetFSTBare");
  SEXP res = R_NilValue;
  const std::string rootPath_alt = as_dirpath(rootPath);
  std::vector<int64_t> dim_alt = as<std::vector<int64_t>>(wrap(dim));
  
  switch(dtype){
  case REALSXP: 
    res = subsetFSTtemplate<REALSXP>(rootPath_alt, dim_alt, parsed);
    break;
  case INTSXP:
    res = subsetFSTtemplate<INTSXP>(rootPath_alt, dim_alt, parsed);
    break;
  case STRSXP:
  case CHARSXP:
    res = subsetFSTtemplate<STRSXP>(rootPath_alt, dim_alt, parsed);
    break;
  case CPLXSXP:
    res = subsetFSTtemplate<CPLXSXP>(rootPath_alt, dim_alt, parsed);
    break;
  default:
    stop("Unknown data type: only numeric, integer, character, and complex arrays are supported - provided SEXPTYPE: " + std::to_string(dtype));
  }
  tok("E subsetFSTBare");
  return res;
}

SEXP subsetFST(const std::string& rootPath, SEXP listOrEnv, const std::vector<int64_t>& dim, SEXPTYPE dtype, SEXP reshape, bool drop){
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  tok("S subsetFST");
  ParsedIndex* tp = parseAndScheduleBlocks(listOrEnv, dim);
  
  SEXP res = subsetFSTBare(rootPath, tp, dim, dtype);
  
  delete tp;
  
  reshapeOrDrop(res, reshape, drop); 
  tok("E subsetFST");
  return res;
}



SEXP scheduleFST(SEXP listOrEnv, const std::vector<int64_t>& dim, bool forceSchedule, int64_t hint){
  tok("S scheduleFST");
  
  if(hint <= 1){
    stop("MARGIN <= 1 is not supported when scheduling slices due to performance issue");
  }
  
  // parse index
  ParsedIndex* subparsed = new ParsedIndex(listOrEnv, dim, true);
  
  if(subparsed->subset_mode == LASUBMOD_SINGLE){
    stop("Single subscript is not supported.");
  }
  
  int64_t ndims = dim.size();
  
  if(subparsed->subset_mode == LASUBMOD_NOIDX){
    subparsed->location_indices.resize(ndims);
    for(int64_t ii = 1; ii < ndims - 1; ii++ ){
      (subparsed->location_indices)[ii] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(), true);
    }
  }
  (subparsed->location_indices)[ndims - 1] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(1,1), false);
  
  // force to be LASUBMOD_MULTI (x[i,j,k], or x[,,k])
  subparsed->subset_mode = LASUBMOD_MULTI;
  
  (subparsed->target_dimension)[ndims - 1] = 1;
  
  int64_t expected_length = std::accumulate(subparsed->target_dimension.begin(), subparsed->target_dimension.end(), 
                                            INTEGER64_ONE, std::multiplies<int64_t>());
  
  subparsed->expected_length = expected_length;
  List re = PROTECT(subparsed->asList());
  ScheduledIndex* schedule = new ScheduledIndex(Shield<SEXP>(wrap(re["location_indices"])), dim, forceSchedule, hint);
  
  subparsed->schedule = schedule;
  
  pre_scheduled = subparsed;
  
  UNPROTECT(1);
  tok("E scheduleFST");
  return wrap(re);
}

SEXP executeScheduleFST(const std::string& rootPath, SEXPTYPE dtype, SEXP reshape, bool drop, int64_t partition){

  if(pre_scheduled == nullptr || pre_scheduled->schedule == nullptr){
    stop("Cannot execute schedules as no slice plan has been scheduled");
  }
  if(partition < 0){
    stop("partition number must be positive integer");
  }
  
  int64_t ndims = pre_scheduled->schedule->dimension.size();
  
  
  std::get<0>((pre_scheduled->location_indices)[ndims-1])[0] = partition;
  pre_scheduled->schedule->partition_index[0] = partition;

  SEXP res = subsetFSTBare(rootPath, pre_scheduled, pre_scheduled->schedule->dimension, dtype);
  
  return reshapeOrDrop(res, reshape, drop); 

}

SEXP scheduleExistsFST(){
  if(pre_scheduled != nullptr){
    ScheduledIndex* s = pre_scheduled->schedule;
    pre_scheduled->schedule = nullptr;
    List res = pre_scheduled->asList();
    pre_scheduled->schedule = s;
    return wrap(res);
  } else {
    return wrap(false);
  }
}

SEXP freeScheduleFST(){
  if(pre_scheduled != nullptr){
    delete pre_scheduled;
  }
  pre_scheduled = nullptr;  
  return R_NilValue;
}
