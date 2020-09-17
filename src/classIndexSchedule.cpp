#include "classIndexSchedule.h"

#include "common.h"
#include "utils.h"
#include "indexConvert.h"
using namespace Rcpp;

ScheduledIndex::ScheduledIndex(
  const bool _block_indexed, 
  const std::vector<int64_t> _dimension,
  const std::vector<std::pair<std::vector<int64_t>, bool>> _block_location,
  const std::vector<int64_t>& _partition_index,
  const std::vector<int64_t> _schedule_index,
  const std::vector<int64_t> _schedule_dimension,
  const std::vector<int64_t> _block_dimension,
  const std::vector<int64_t> _block_schedule,
  const int64_t _block_schedule_start,
  const int64_t _block_schedule_end
):
  block_indexed(_block_indexed),
  dimension(_dimension),
  schedule_index(_schedule_index),
  schedule_dimension(_schedule_dimension),
  block_dimension(_block_dimension),
  block_schedule(_block_schedule),
  block_schedule_start(_block_schedule_start),
  block_schedule_end(_block_schedule_end),
  block_location(_block_location)
{ 
  this->partition_index = std::vector<int64_t>(_partition_index.begin(), _partition_index.end());
  
  
  partition_counts = partition_index.size();
  schedule_counts_per_part = schedule_index.size();
  block_ndims = block_dimension.size();
  
  block_prod_dim = std::vector<int64_t>(block_ndims, 1);
  block_expected_length = 1;
  
  for(R_xlen_t ii = 0; ii < block_ndims; ii++){
    if(ii >= 1){
      block_prod_dim[ii] = block_prod_dim[ii-1] * block_dimension[ii - 1];
    }
    if(std::get<1>(block_location[ii])){
      block_expected_length *= dimension[ii];
    } else {
      block_expected_length *= std::get<0>(block_location[ii]).size();
    }
  }
  block_length = block_prod_dim[block_ndims-1] * block_dimension[block_ndims-1];
}

ScheduledIndex::ScheduledIndex(SEXP locations, const std::vector<int64_t>& dim, bool forceSchedule, int64_t hint){
  int64_t ndims = dim.size();
  
  stopIfNot(
    Rf_xlength(locations) == ndims && ndims >= 2,
    "`scheduleIndexing`: locations and dim have different sizes or dimension size is less than 2"
  );
  
  // Find split
  // dim -> dim1 x dim2 length(dim1) <= length(dim)-1
  // std::vector<int64_t> dim_int64 = numeric2Int64t(dim);
  int64_t block_size = 1;
  int64_t buffer_margin = 0;
  if(hint <= 1 || hint > ndims ){
    if(hint > -1){
      // hint is set, but wrong
      warning("Scheduling index block: `MARGIN` must be strictly greater than 1 and less equal than the total dimensions");
    }
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(block_size > getLazyBlockSize()){
        break;
      }
      block_size *= dim[buffer_margin];
    }
  } else {
    for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
      if(block_size > getLazyBlockSize() || buffer_margin >= (hint - 1)){
        break;
      }
      block_size *= dim[buffer_margin];
    }
  }
  
  
  
  // sub-block size is buffer_margin
  std::vector<int64_t> block_dim = std::vector<int64_t>(dim.begin(), dim.begin() + buffer_margin);
  int64_t block_length = std::accumulate(block_dim.begin(), block_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  std::vector<int64_t> block_dim_prod(buffer_margin);
  block_dim_prod[0] = 1;
  for(R_xlen_t ii = 1; ii < buffer_margin; ii++){
    block_dim_prod[ii] = block_dim_prod[ii-1] * block_dim[ii - 1];
  }
  std::vector<int64_t> rest_dim = std::vector<int64_t>(dim.begin() + buffer_margin, dim.end());
  SEXP rest_loc = PROTECT(Rf_allocVector(VECSXP, ndims - buffer_margin));
  
  // std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim);
  for(R_xlen_t ii = buffer_margin; ii < ndims - 1; ii++){
    SEXP loc_ii = VECTOR_ELT(locations, ii);
    SET_VECTOR_ELT(rest_loc, ii - buffer_margin, loc_ii);
  }
  
  SET_VECTOR_ELT(rest_loc, ndims - buffer_margin - 1, wrap(1));
  rest_dim[ndims - buffer_margin - 1] = 1;
  
  std::vector<int64_t> rest_idx = loc2idx3(rest_loc, rest_dim);
  rest_dim[ndims - buffer_margin - 1] = dim[ndims - 1];
  UNPROTECT(1);
  
  // generate index set for block
  std::vector<int64_t> subblock_idx(0);
  bool buffer_expanded = false;
  int64_t subblock_idx_min = 1;
  int64_t subblock_idx_max = block_length;
  int64_t subblock_idx_size = block_length;
  std::vector<std::pair<std::vector<int64_t>, bool>> buffer_loc_vec;
  SEXP buffer_loc;
  if(buffer_margin >= 2 && (block_size < BLOCKLARGE || forceSchedule)){
    buffer_expanded = true;
    buffer_loc_vec = std::vector<std::pair<std::vector<int64_t>, bool>>(buffer_margin);
    buffer_loc = PROTECT(Rf_allocVector(VECSXP, buffer_margin));
    for(R_xlen_t ii = 0; ii < buffer_margin; ii++){
      SEXP loc_ii = VECTOR_ELT(locations, ii);
      if(loc_ii == R_MissingArg){
        buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
      } else {
        buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(loc_ii), false);
      }
      SET_VECTOR_ELT(buffer_loc, ii, loc_ii);
    }
    subblock_idx = loc2idx3(buffer_loc, block_dim);
    UNPROTECT(1);
    
    // calculate min and max
    subblock_idx_min = block_length;
    subblock_idx_max = 1;
    subblock_idx_size = subblock_idx.size();
    for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++){
      if((*ptr_subblock_idx) != NA_REAL && (*ptr_subblock_idx) != NA_INTEGER64){
        if(*ptr_subblock_idx > subblock_idx_max){
          subblock_idx_max = *ptr_subblock_idx;
        }
        if(*ptr_subblock_idx < subblock_idx_min){
          subblock_idx_min = *ptr_subblock_idx;
        }
      }
    }
    
  } else {
    subblock_idx_size = 1;
    buffer_loc_vec = std::vector<std::pair<std::vector<int64_t>, bool>>(buffer_margin);
    // buffer_loc = PROTECT(Rf_allocVector(VECSXP, buffer_margin));
    for(R_xlen_t ii = 0; ii < buffer_margin; ii++){
      SEXP loc_ii = VECTOR_ELT(locations, ii);
      // SET_VECTOR_ELT(buffer_loc, ii, loc_ii);
      if(loc_ii == R_MissingArg){
        subblock_idx_size *= dim[ii];
        buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
      } else {
        subblock_idx_size *= Rf_xlength(loc_ii);
        buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(loc_ii), false);
      }
    }
    // UNPROTECT(1);
  }
  
  std::vector<int64_t> partition_id;
  // int64_t nblocks = rest_idx.size();
  int64_t nparts;
  
  SEXP partition_margin = VECTOR_ELT(locations, ndims - 1);
  
  if(partition_margin == R_MissingArg){
    nparts = *(dim.end() - 1);
    partition_id = seq_len3<int64_t>(nparts);
  } else {
    partition_id = as<std::vector<int64_t>>(partition_margin);
    nparts = partition_id.size();
  }
  
  //   subblock_idx_min, subblock_idx_max
  // );
  
  // List re = List::create(
  //   _["dimension"] = dim,
  //   _["partition_index"] = partition_id,
  //   _["partition_counts"] = nparts,
  //   // number of blocks and index to schedule
  //   _["schedule_counts_per_part"] = nblocks,
  //   _["schedule_index"] = rest_idx,
  //   _["schedule_dimension"] = rest_dim,
  //
  //   // block information
  //   _["block_ndims"] = buffer_margin,
  //   _["block_dimension"] = block_dim,
  //   _["block_prod_dim"] = block_dim_prod,
  //   _["block_schedule"] = subblock_idx,
  //   _["block_schedule_start"] = subblock_idx_min,
  //   _["block_schedule_end"] = subblock_idx_max,
  //   _["block_length"] = block_length,
  //   _["block_expected_length"] = subblock_idx_size,
  //   _["block_indexed"] = buffer_expanded,
  //   _["block_location"] = buffer_loc
  //
  //   // _["expected_length"] = expected_length,
  //   // _["target_dimension"] = target_dimension
  // );
  this->block_indexed = buffer_expanded;
  this->dimension = dim;
  
  // partition level
  this->partition_counts = nparts;
  this->partition_index = partition_id;
  
  // schedule level
  this->schedule_counts_per_part = rest_idx.size();
  this->schedule_index = rest_idx;      // indices to schedule run blocks
  this->schedule_dimension = rest_dim; // [schedule dim, partition counts]
  
  // block level
  this->block_ndims=buffer_margin;                    // length(block dim)
  this->block_dimension=block_dim; // [block dim], full version
  this->block_prod_dim=block_dim_prod; // prod([1, block dim]), used to locate indices when block is too large to index
  this->block_schedule=subblock_idx; // given a flattened block (full version), which indices to subset?
  this->block_schedule_start=subblock_idx_min;
  this->block_schedule_end=subblock_idx_max;      // min, max of block_schedule
  
  this->block_length=block_length;                  // # elements in a block (full version) = prod(block_dimension)
  this->block_expected_length=subblock_idx_size;// # elements in a block (subset version) = length(block_schedule)
  
  this->block_location=buffer_loc_vec;           // subset of locational indices of blocks
  
  
  // Split dimension by two:
}


Rcpp::List ScheduledIndex::asList() {
  SEXP tmp = PROTECT(Rf_allocVector(VECSXP, block_location.size()));
  NumericVector el;
  for(size_t i=0; i < block_location.size(); i++){
    if(std::get<1>(block_location[i])){
      // Missing value
      SET_VECTOR_ELT(tmp, i, R_MissingArg);
    } else {
      el = wrap(std::get<0>(block_location[i]));
      el[ el == NA_INTEGER64 ] = NA_REAL;
      SET_VECTOR_ELT(tmp, i, el);
    }
  }
  el = wrap(partition_index);
  el[ el == NA_INTEGER64 ] = NA_REAL;
  
  List re = Rcpp::List::create(
    _["dimension"] = (wrap(dimension)),
    _["partition_index"] = wrap(el),
    _["partition_counts"] = (wrap(partition_counts)),
    // number of blocks and index to schedule
    _["schedule_counts_per_part"] = (wrap(schedule_counts_per_part)),
    _["schedule_index"] = (wrap(schedule_index)),
    _["schedule_dimension"] = (wrap(schedule_dimension)),
    
    // block information
    _["block_ndims"] = (wrap(block_ndims)),
    _["block_dimension"] = (wrap(block_dimension)),
    _["block_prod_dim"] = (wrap(block_prod_dim)),
    _["block_schedule"] = (wrap(block_schedule)),
    _["block_schedule_start"] = (wrap(block_schedule_start)),
    _["block_schedule_end"] = (wrap(block_schedule_end)),
    _["block_length"] = (wrap(block_length)),
    _["block_expected_length"] = (wrap(block_expected_length)),
    _["block_indexed"] = (wrap(block_indexed)),
    _["block_location"] = tmp
  );
  UNPROTECT(1);
  return re;
}

ParsedIndex::ParsedIndex(const SEXP listOrEnv, const std::vector<int64_t>& dim, bool pos_subscript){
  // tok("S subsetIdx2");
  int n_protected = 0;
  // List location_idx = List::create();
  
  
  tok("S ParsedIndex::ParsedIndex");
  List sliceIdx;  // VECSXP
  
  switch(TYPEOF(listOrEnv)) {
  case ENVSXP: {
    SEXP dots = Rf_findVarInFrame(listOrEnv, R_DotsSymbol);
    int64_t idx_size = 0;
    R_xlen_t ndims = dim.size();
    sliceIdx = List::create();
    for(; dots != R_NilValue & dots != R_MissingArg; dots = CDR(dots), idx_size++ ){
      if(idx_size >= ndims){
        stop("Incorrect subscript dimensions, required: 0, 1, ndim.");
      }
      sliceIdx.push_back(CAR(dots));
    }
    break;
  }
  case VECSXP:
    sliceIdx = as<List>(listOrEnv);
    break;
  default:
    Rcpp::stop("Input `listOrEnv` must be either a list of indices or an environment");
  }
  
  R_xlen_t ndims = dim.size();
  std::vector<std::pair<std::vector<int64_t>, bool>> location_idx(ndims);
  
  // mode:
  // 0: x[i,j,k,l,...]
  // 1: x[i]
  // 2: x[]
  subset_mode = LASUBMOD_NOIDX;
  R_xlen_t idx_size = 0;
  SEXP el;
  
  // used to estimate expected dimension
  
  std::vector<int64_t> target_dim = std::vector<int64_t>(dim.begin(), dim.end());
  std::vector<bool> neg_subscr = std::vector<bool>(ndims, false);
  
  int64_t di;
  NumericVector sidx;
  NumericVector neg_sidx;
  SEXP first_el;
  bool first_error = false;
  
  for(; idx_size < sliceIdx.size(); idx_size++ ){
    if( idx_size > ndims ){
      stop("Incorrect dimension while subsetting an array");
    }
    
    el = PROTECT(sliceIdx[idx_size]);
    n_protected++;
    
    // el might be promise SEXP, if so, evaluate
    if ( TYPEOF(el) == PROMSXP ){
      // This is a promise, need to evaluate
      el = Rf_eval( PREXPR(el), PRENV( el ));
    }
    
    // store the first element for future use
    if(idx_size == 0){
      first_el = el;
    }
    
    // current margin size
    di = dim[ idx_size ];
    
    // Check if el is missing value, this means
    if( el == R_MissingArg ){
      
      neg_subscr[ idx_size ] = false;
      target_dim[ idx_size ] = di;
      
      location_idx[ idx_size ] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
      continue;
    }
    
    sidx = as<NumericVector>(el);
    
    neg_sidx = sidx[ !(is_na(sidx) | sidx >= 0) ];
    
    
    if( neg_sidx.size() > 0 ){
      if( is_true( any( !(is_na(sidx) | sidx <= 0) ) ) ){
        stop("only 0's may be mixed with negative subscripts");
      }
      neg_sidx = neg_sidx * (-1);
      sidx = sort_unique( neg_sidx );
      sidx = sidx[sidx <= di];
      if( pos_subscript ){
        NumericVector tmp = no_init( di - sidx.size() );
        NumericVector::iterator ptr_neg_sidx = tmp.begin();
        NumericVector::iterator ptr_sidx = sidx.begin();
        for(int64_t el = 1; el <= di & ptr_neg_sidx != tmp.end(); el++){
          if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
            ptr_sidx++;
          } else {
            *ptr_neg_sidx++ = el;
          }
        }
        
        sidx = tmp;
        neg_subscr[ idx_size ] = false;
        target_dim[ idx_size ] = sidx.size();
      } else {
        neg_subscr[ idx_size ] = true;
        target_dim[ idx_size ] = di - sidx.size();
      }
      
    } else {
      
      if( is_true( any( !(is_na(sidx) | sidx <= di) ) ) ){
        if(idx_size == 0){
          // in subset mode 1, indices can overflow dim1, but it can't overflow total length
          // flag the error and deal with it later
          first_error = true;
        } else {
          stop("incorrect number of dimensions");
        }
        
      }
      
      sidx = sidx[ is_na(sidx) | sidx > 0];
      
      neg_subscr[ idx_size ] = false;
      target_dim[ idx_size ] = sidx.size();
    }
    
    location_idx[ idx_size ] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(sidx), false);
  }
  if(idx_size == 0){
    subset_mode = LASUBMOD_NOIDX;      // 2
  } else if(idx_size == 1){
    if(first_el == R_MissingArg){
      subset_mode = LASUBMOD_NOIDX;    // 2
    } else {
      subset_mode = LASUBMOD_SINGLE;   // 1
      
      // need to re-calculate first_el as all previous filterings are probably wrong
      sidx = as<NumericVector>(first_el);
      
      neg_sidx = sidx[ !(is_na(sidx) | sidx >= 0) ];
      int64_t total_length = std::accumulate(dim.begin(), dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
      if( neg_sidx.size() > 0 ){
        // already checked
        // if( is_true( any( !(is_na(sidx) | sidx <= 0) ) ) ){
        //   stop("only 0's may be mixed with negative subscripts");
        // }
        neg_sidx = neg_sidx * (-1);
        sidx = sort_unique( neg_sidx );
        sidx = sidx[sidx <= total_length];
        if( pos_subscript ){
          NumericVector tmp = no_init( total_length - sidx.size() );
          NumericVector::iterator ptr_neg_sidx = tmp.begin();
          NumericVector::iterator ptr_sidx = sidx.begin();
          for(int64_t el = 1; el <= total_length & ptr_neg_sidx != tmp.end(); el++){
            if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
              ptr_sidx++;
            } else {
              *ptr_neg_sidx++ = el;
            }
          }
          
          sidx = tmp;
          neg_subscr[ 0 ] = false;
          target_dim[ 0 ] = sidx.size();
        } else {
          neg_subscr[ 0 ] = true;
          target_dim[ 0 ] = total_length - sidx.size();
        }
        
      } else {
        
        // if( is_true( any( !(is_na(sidx) | sidx == NA_INTEGER64 | sidx <= total_length) ) ) ){
        //   stop("incorrect size of subscript: exceed max length");
        // }
        sidx = sidx[ is_na(sidx) | sidx == NA_INTEGER64 | sidx > 0 ];
        
        sidx[ is_na(sidx) | sidx == NA_INTEGER64 | sidx > total_length ] = NA_REAL;
        
        neg_subscr[ 0 ] = false;
        target_dim[ 0 ] = sidx.size();
      }
      
      location_idx[ 0 ] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(sidx), false);
      location_idx.resize(1);
      this->expected_length = target_dim[0];
    }
  } else if (ndims != idx_size){
    stop("Dimension mismatch while subseting an array");
  } else {
    subset_mode = LASUBMOD_MULTI;      // 0
  }
  
  if(subset_mode != LASUBMOD_SINGLE){
    if(first_error){
      stop("incorrect number of first dimension");
    }
    this->expected_length = std::accumulate(target_dim.begin(), target_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  }
  if(subset_mode == LASUBMOD_NOIDX){
    // print(wrap(location_idx.size()));
    location_idx[ 0 ] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
    location_idx.resize(1);
  }
  
  this->target_dimension = target_dim;
  this->negative_subscript = neg_subscr;
  this->location_indices = location_idx;
  
  this->schedule = nullptr;
  
  
  if(n_protected > 0){
    UNPROTECT(n_protected);
  }
  tok("E ParsedIndex::ParsedIndex");
}

Rcpp::List ParsedIndex::asList() {
  List tmp = List::create();
  for(size_t i=0; i < location_indices.size(); i++){
    if(std::get<1>(location_indices[i])){
      // Missing value
      tmp.push_back(R_MissingArg);
    } else {
      NumericVector sub = as<NumericVector>(wrap(std::get<0>(location_indices[i])));
      sub[ sub == NA_INTEGER64 ] = NA_REAL;
      tmp.push_back(sub);
    }
  }
  SEXP sch = R_NilValue;
  if(schedule != nullptr){
    sch = (wrap(schedule->asList()));
  }
  
  return List::create(
    _["subset_mode"] = (wrap(subset_mode)),
    _["target_dimension"] = (wrap(target_dimension)),
    _["expected_length"] = (wrap(expected_length)),
    _["location_indices"] = tmp,
    _["negative_subscript"] = (wrap(negative_subscript)),
    _["schedule"] = sch
  );
}
