#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
#include "indexConvert.h"
using namespace Rcpp;

// std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim){
// 
//   // Check whether parent_dim matches with location index size - validation
//   R_xlen_t ndim = parent_dim.size();
// 
//   if( ndim != Rf_xlength(locations) ){
//     stop("Dimension input wrong for `loc2idx2`");
//   }
// 
//   // Get sub-dimension for location indexes (the dimension of returned value)
//   std::vector<int64_t> sub_dim(ndim);
//   std::vector<std::vector<int64_t>> location_copy(ndim);
// 
//   for(R_xlen_t dd = 0; dd < ndim; dd++){
//     SEXP subloc = VECTOR_ELT(locations, dd);
//     if(subloc == R_MissingArg){
//       sub_dim[dd] = parent_dim[dd];
// 
//       std::vector<int64_t> current_location(sub_dim[dd]);
//       std::iota(current_location.begin(), current_location.end(), 1);
//       location_copy[dd] = current_location;
//     } else {
//       sub_dim[dd] = Rf_xlength(subloc);
//       location_copy[dd] = as<std::vector<int64_t>>(subloc);
//     }
//   }
// 
//   // Total length to return
//   int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
// 
//   // Generate integer vector to be returned and assign dimension
//   std::vector<int64_t> re(sub_size, 1);
// 
//   if( sub_size == 0 ){
//     return re;
//   }
// 
//   // Inflate indexes and add them
//   R_xlen_t ii = 0, jj = 0;
//   R_xlen_t inflate = 1;
//   R_xlen_t tmp = 0;
//   R_xlen_t neach = 1;
// 
//   for(ii = 0; ii < ndim; ii++ ){
// 
//     tmp = parent_dim[ii];
// 
//     // Ger slice index
//     std::vector<int64_t> current_location = location_copy[ii];
// 
//     // Assign invalid indexes to NA
//     // The first needs to be is_na otherwise error will be raised: "can't subset using a logical vector with NAs"
//     // current_location[ is_na(current_location) | current_location < 1 | current_location > tmp ] = NA_INTEGER;
// 
//     // write to re,
//     // re += rep(current_location, nrepeat, neach)
//     // if( neach > 1 ){
//     //   current_location = Rcpp::rep_each(current_location, neach);
//     // }
//     // re += Rcpp::rep_len(current_location, sub_size);
//     std::vector<int64_t>::iterator ptr_current_location = current_location.begin();
// 
//     for( std::vector<int64_t>::iterator ptr_re = re.begin(); ptr_re != re.end(); ){
//       if(ptr_current_location == current_location.end()){
//         ptr_current_location = current_location.begin();
//       }
//       for(jj = 0; jj < neach; jj++){
//         // if re[...] is not NA
//         if(*ptr_re != NA_REAL &&
//            *ptr_re != NA_INTEGER64 &&
// 
//            // current_location is not NA
//            *ptr_current_location != NA_REAL &&
//            *ptr_current_location != NA_INTEGER64 &&
// 
//            // current_location is valid, i.e. current_location >= 1 & current_location <= tmp
//            *ptr_current_location >= 1 &&
// 
//            *ptr_current_location <= tmp
// 
//         ){
// 
//           // index[[...]] += (locations[[ii]] - 1) * inflate
//           *ptr_re += ((*ptr_current_location) - 1) * inflate;
//         } else {
//           *ptr_re = NA_REAL;
//         }
//         ptr_re++;
//       }
//       if(ptr_current_location != current_location.end()){
//         ptr_current_location++;
//       }
//     }
//     // Prepare for next loop
//     inflate = inflate * tmp;
//     neach = neach * current_location.size();
// 
//   }
// 
//   return(re);
// }

// // [[Rcpp::export]]
// SEXP subsetIdx2(const Rcpp::List sliceIdx, const std::vector<int64_t>& dim, bool pos_subscript){
//   // tok("S subsetIdx2");
//   int n_protected = 0;
//   // List location_idx = List::create();
// 
//   R_xlen_t ndims = dim.size();
//   std::vector<std::pair<std::vector<int64_t>, bool>> location_idx(ndims);
// 
//   // mode:
//   // 0: x[i,j,k,l,...]
//   // 1: x[i]
//   // 2: x[]
//   int subset_mode = LASUBMOD_NOIDX;
//   R_xlen_t idx_size = 0;
//   SEXP el;
// 
//   // used to estimate expected dimension
//   // int64_t total_length = std::accumulate(dim.begin(), dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
//   std::vector<int64_t> target_dim = std::vector<int64_t>(dim.begin(), dim.end());
//   std::vector<bool> neg_subscr = std::vector<bool>(ndims, false);
// 
//   int64_t di;
//   NumericVector sidx;
//   NumericVector neg_sidx;
// 
//   for(; idx_size < sliceIdx.size(); idx_size++ ){
//     if( idx_size > ndims ){
//       stop("Incorrect dimension while subsetting an array");
//     }
// 
//     el = sliceIdx[idx_size];
// 
//     // el might be promise SEXP, if so, evaluate
//     if ( TYPEOF(el) == PROMSXP ){
//       // This is a promise, need to evaluate
//       el = Rf_eval( PREXPR(el), PRENV( el ));
//     }
// 
// 
//     // current margin size
//     di = dim[ idx_size ];
// 
//     // Check if el is missing value, this means
//     if( el == R_MissingArg ){
// 
//       neg_subscr[ idx_size ] = false;
//       target_dim[ idx_size ] = di;
// 
//       location_idx[ idx_size ] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
//       continue;
//     }
// 
//     sidx = as<NumericVector>(el);
// 
//     neg_sidx = sidx[ !(is_na(sidx) | sidx >= 0) ];
// 
// 
//     if( neg_sidx.size() > 0 ){
//       if( is_true( any( !(is_na(sidx) | sidx <= 0) ) ) ){
//         stop("only 0's may be mixed with negative subscripts");
//       }
//       neg_sidx = neg_sidx * (-1);
//       sidx = sort_unique( neg_sidx );
//       sidx = sidx[sidx <= di];
//       if( pos_subscript ){
//         NumericVector tmp = no_init( di - sidx.size() );
//         NumericVector::iterator ptr_neg_sidx = tmp.begin();
//         NumericVector::iterator ptr_sidx = sidx.begin();
//         for(int64_t el = 1; el <= di & ptr_neg_sidx != tmp.end(); el++){
//           if( ptr_sidx != sidx.end() && el - *ptr_sidx >= 0 ){
//             ptr_sidx++;
//           } else {
//             *ptr_neg_sidx++ = el;
//           }
//         }
// 
//         sidx = tmp;
//         neg_subscr[ idx_size ] = false;
//         target_dim[ idx_size ] = sidx.size();
//       } else {
//         neg_subscr[ idx_size ] = true;
//         target_dim[ idx_size ] = di - sidx.size();
//       }
// 
//     } else {
// 
//       if( is_true( any( !(is_na(sidx) | sidx <= di) ) ) ){
//         stop("incorrect number of dimensions");
//       }
// 
//       sidx = sidx[ is_na(sidx) | sidx > 0];
// 
//       neg_subscr[ idx_size ] = false;
//       target_dim[ idx_size ] = sidx.size();
//     }
// 
//     location_idx[ idx_size ] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(sidx), true);
//   }
//   if(idx_size == 0){
//     subset_mode = LASUBMOD_NOIDX;
//   } else if(idx_size == 1){
//     if(std::get<1>(location_idx[0])){
//       subset_mode = LASUBMOD_NOIDX;
//     } else {
//       subset_mode = LASUBMOD_SINGLE;
//     }
//   } else if (ndims != idx_size){
//     stop("Dimension mismatch while subseting an array");
//   } else {
//     subset_mode = LASUBMOD_MULTI;
//   }
// 
//   ParsedIndex parsed(subset_mode, target_dim, neg_subscr, location_idx, nullptr);
// 
//   if(n_protected > 0){
//     UNPROTECT(n_protected);
//   }
//   tok("E subsetIdx2");
//   // return List::create(
//   //   _["subset_mode"] = subset_mode,
//   //   _["target_dimension"] = target_dim,
//   //   _["expected_length"] = expected_len,
//   //   _["location_indices"] = location_idx,
//   //   _["negative_subscript"] = neg_subscr
//   // );
//   return parsed.asList();
// }

// // [[Rcpp::export]]
// ScheduledIndex scheduleIndexing(SEXP locations, const std::vector<int64_t>& dim, bool forceSchedule){
//   int64_t ndims = dim.size();
// 
//   stopIfNot(
//     Rf_xlength(locations) == ndims && ndims >= 2,
//     "`scheduleIndexing`: locations and dim have different sizes or dimension size is less than 2"
//   );
// 
//   // Find split
//   // dim -> dim1 x dim2 length(dim1) <= length(dim)-1
//   // std::vector<int64_t> dim_int64 = numeric2Int64t(dim);
//   int64_t block_size = 1;
//   int64_t buffer_margin = 0;
//   for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
//     if(block_size > getLazyBlockSize()){
//       break;
//     }
//     block_size *= dim[buffer_margin];
//   }
// 
//   // sub-block size is buffer_margin
//   std::vector<int64_t> block_dim = std::vector<int64_t>(dim.begin(), dim.begin() + buffer_margin);
//   int64_t block_length = std::accumulate(block_dim.begin(), block_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
//   std::vector<int64_t> block_dim_prod(buffer_margin);
//   block_dim_prod[0] = 1;
//   for(R_xlen_t ii = 1; ii < buffer_margin; ii++){
//     block_dim_prod[ii] = block_dim_prod[ii-1] * block_dim[ii - 1];
//   }
//   std::vector<int64_t> rest_dim = std::vector<int64_t>(dim.begin() + buffer_margin, dim.end());
//   SEXP rest_loc = PROTECT(Rf_allocVector(VECSXP, ndims - buffer_margin));
// 
//   // std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim);
//   for(R_xlen_t ii = buffer_margin; ii < ndims - 1; ii++){
//     SEXP loc_ii = VECTOR_ELT(locations, ii);
//     SET_VECTOR_ELT(rest_loc, ii - buffer_margin, loc_ii);
//   }
// 
//   SET_VECTOR_ELT(rest_loc, ndims - buffer_margin - 1, wrap(1));
//   rest_dim[ndims - buffer_margin - 1] = 1;
// 
//   std::vector<int64_t> rest_idx = loc2idx3(rest_loc, rest_dim);
//   rest_dim[ndims - buffer_margin - 1] = dim[ndims - 1];
//   UNPROTECT(1);
// 
//   // generate index set for block
//   std::vector<int64_t> subblock_idx(0);
//   bool buffer_expanded = false;
//   int64_t subblock_idx_min = 1;
//   int64_t subblock_idx_max = block_length;
//   int64_t subblock_idx_size = block_length;
//   std::vector<std::pair<std::vector<int64_t>, bool>> buffer_loc_vec;
//   SEXP buffer_loc;
//   if(buffer_margin >= 2 && (block_size < BLOCKLARGE || forceSchedule)){
//     buffer_expanded = true;
//     buffer_loc_vec = std::vector<std::pair<std::vector<int64_t>, bool>>(buffer_margin);
//     buffer_loc = PROTECT(Rf_allocVector(VECSXP, buffer_margin));
//     for(R_xlen_t ii = 0; ii < buffer_margin; ii++){
//       SEXP loc_ii = VECTOR_ELT(locations, ii);
//       if(loc_ii == R_MissingArg){
//         buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
//       } else {
//         buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(loc_ii), false);
//       }
//       SET_VECTOR_ELT(buffer_loc, ii, loc_ii);
//     }
//     subblock_idx = loc2idx3(buffer_loc, block_dim);
//     UNPROTECT(1);
// 
//     // calculate min and max
//     subblock_idx_min = block_length;
//     subblock_idx_max = 1;
//     subblock_idx_size = subblock_idx.size();
//     for(std::vector<int64_t>::iterator ptr_subblock_idx = subblock_idx.begin(); ptr_subblock_idx != subblock_idx.end(); ptr_subblock_idx++){
//       if((*ptr_subblock_idx) != NA_REAL && (*ptr_subblock_idx) != NA_INTEGER64){
//         if(*ptr_subblock_idx > subblock_idx_max){
//           subblock_idx_max = *ptr_subblock_idx;
//         }
//         if(*ptr_subblock_idx < subblock_idx_min){
//           subblock_idx_min = *ptr_subblock_idx;
//         }
//       }
//     }
// 
//   } else {
//     subblock_idx_size = 1;
//     buffer_loc_vec = std::vector<std::pair<std::vector<int64_t>, bool>>(buffer_margin);
//     // buffer_loc = PROTECT(Rf_allocVector(VECSXP, buffer_margin));
//     for(R_xlen_t ii = 0; ii < buffer_margin; ii++){
//       SEXP loc_ii = VECTOR_ELT(locations, ii);
//       // SET_VECTOR_ELT(buffer_loc, ii, loc_ii);
//       if(loc_ii == R_MissingArg){
//         subblock_idx_size *= dim[ii];
//         buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(std::vector<int64_t>(0), true);
//       } else {
//         subblock_idx_size *= Rf_xlength(loc_ii);
//         buffer_loc_vec[ii] = std::pair<std::vector<int64_t>, bool>(as<std::vector<int64_t>>(loc_ii), false);
//       }
//     }
//     UNPROTECT(1);
//   }
// 
//   std::vector<int64_t> partition_id;
//   // int64_t nblocks = rest_idx.size();
//   int64_t nparts;
// 
//   SEXP partition_margin = VECTOR_ELT(locations, ndims - 1);
// 
//   if(partition_margin == R_MissingArg){
//     nparts = *(dim.end() - 1);
//     partition_id = seq_len3<int64_t>(nparts);
//   } else {
//     partition_id = as<std::vector<int64_t>>(partition_margin);
//     nparts = partition_id.size();
//   }
// 
//   ScheduledIndex schedule(
//     buffer_expanded, dim, buffer_loc_vec, partition_id,
//     rest_idx, rest_dim, block_dim, subblock_idx,
//     subblock_idx_min, subblock_idx_max
//   );
// 
//   // List re = List::create(
//   //   _["dimension"] = dim,
//   //   _["partition_index"] = partition_id,
//   //   _["partition_counts"] = nparts,
//   //   // number of blocks and index to schedule
//   //   _["schedule_counts_per_part"] = nblocks,
//   //   _["schedule_index"] = rest_idx,
//   //   _["schedule_dimension"] = rest_dim,
//   //
//   //   // block information
//   //   _["block_ndims"] = buffer_margin,
//   //   _["block_dimension"] = block_dim,
//   //   _["block_prod_dim"] = block_dim_prod,
//   //   _["block_schedule"] = subblock_idx,
//   //   _["block_schedule_start"] = subblock_idx_min,
//   //   _["block_schedule_end"] = subblock_idx_max,
//   //   _["block_length"] = block_length,
//   //   _["block_expected_length"] = subblock_idx_size,
//   //   _["block_indexed"] = buffer_expanded,
//   //   _["block_location"] = buffer_loc
//   //
//   //   // _["expected_length"] = expected_length,
//   //   // _["target_dimension"] = target_dimension
//   // );
//   return schedule;
// 
//   // Split dimension by two:
// }


/*** R

scheduleIndexing(list(1,2,3), c(2,3,4), TRUE)

# subsetIdx2(list(1,2,3), c(2,3,4), TRUE)
*/
