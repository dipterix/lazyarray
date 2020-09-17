#include "saver2.h"

#include "common.h"
#include "saver2ext.h"
#include "indexConvert.h"
#include "fstWrapper.h"
using namespace Rcpp;

// template <typename T>
// std::vector<T> seq_len3(int64_t n){
//   std::vector<T> re = std::vector<T>(n);
//   T v = 1;
//   for(auto it = re.begin(); it != re.end(); it++){
//     *it = v++;
//   }
//   return re;
// }


template <SEXPTYPE RTYPE>
SEXP writeFstPartition(const Rcpp::Vector<RTYPE>& values, const std::string& file, 
                       const std::vector<int64_t>& dim, const Rcpp::List& subparsed,
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
  int64_t nfiles = *(dim.end() - 1);
  
  std::vector<int64_t> dim_part = std::vector<int64_t>(dim.begin(), dim.end() - 1);
  int64_t expected_nrows = std::accumulate(dim_part.begin(), dim_part.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  List tbl;
  
  // make index when subset_mode is 0
  bool make_idx = false;
  if(subset_mode == 2){
    for(int64_t ii = 0; ii < nfiles; ii++ ){
      Vector<RTYPE> sub_values = Vector<RTYPE>(
        values.begin() + (ii * expected_nrows),
        values.begin() + (ii * expected_nrows + expected_nrows)
      );
      std::string fstfile = file + std::to_string(ii + 1) + ".fst";
      if( RTYPE == CPLXSXP){
        tbl = List::create(
          _["V1R"] = Rcpp::Re(static_cast<ComplexVector>(sub_values)),
          _["V1I"] = Rcpp::Im(static_cast<ComplexVector>(sub_values))
        );
      } else {
        tbl = List::create(_["V1"] = sub_values);
      }
      fstStore(fstfile, wrap(tbl), wrap(compression), wrap(uniformEncoding));
    }
    return wrap(-1);
  } else if(subset_mode == 1){
    stop("Single subscript assign (x[i]<-value) has not yet implemented. Please use full subscript (x[,...,i]<-value) or no subscript (x[]<-value)");
  } else {
    for(R_xlen_t ii=0; ii<ndims; ii++ ){
      if(location_indices[ii] != R_MissingArg){
        make_idx = true;
        break;
      }
    }
  }
  
  std::vector<int64_t> row_idx(0);
  
  if( make_idx ){
    const List location_indices_part = List(location_indices.begin(), location_indices.end()-1);
    row_idx = loc2idx3(wrap(location_indices_part), dim_part);
  }
  
  SEXP partition_loc = location_indices[ndims - 1];
  std::vector<int64_t> partition_loc_alt;
  if(partition_loc == R_MissingArg){
    partition_loc_alt = seq_len3<int64_t>(dim[ndims - 1]);
  } else {
    partition_loc_alt = as<std::vector<int64_t>>(partition_loc);
  }
  R_xlen_t nparts = partition_loc_alt.size();
  
  // values could be length < required, need loops
  // Vector<RTYPE> v(values);
  auto ptr_v = values.begin();
  R_xlen_t ii_v = 0;
  bool is_complex = (RTYPE == CPLXSXP);
  StringVector cname;
  if(is_complex){
    cname = {"V1R", "V1I"};
  } else {
    cname = {"V1"};
  }
  
  Vector<RTYPE> partition = static_cast<Vector<RTYPE>>(no_init(expected_nrows));
  auto ptr_partition = partition.begin();
  std::string fstfile;
  int64_t part;
  List tmp;
  
  
  for(R_xlen_t part_ii = 0; part_ii < nparts; part_ii++ ){
    
    part = partition_loc_alt[part_ii];
    
    // skip as part number is invalid
    if(part == NA_REAL || part == NA_INTEGER64 || part < 1 || part > nfiles){
      ii_v += expected_nrows;
      ii_v = ii_v % value_len;
      ptr_v = values.begin() + ii_v;
    }
    
    fstfile = file + std::to_string(part) + ".fst";
    
    // if not make index x[] <- v or x[,,i] <- v or x[,,,] <- v
    if(make_idx){
      if(checkFstMeta(fstfile, expected_nrows, cname)){
        tmp = fstRetrieve(fstfile, wrap(cname), wrap(1), R_NilValue);
        tmp = tmp["resTable"];
        if(is_complex){
          setReIm(static_cast<ComplexVector>(partition), as<NumericVector>(tmp[ "V1R" ]), true);
          setReIm(static_cast<ComplexVector>(partition), as<NumericVector>(tmp[ "V1I" ]), false);
        } else {
          partition = as<Vector<RTYPE>>(tmp[ "V1" ]);
        }
      } else {
        partition.fill( Vector<RTYPE>::get_na() );
      }
      
      std::vector<int64_t>::iterator ptr_row_idx = row_idx.begin();
      for(; ptr_row_idx != row_idx.end(); ptr_row_idx++){
        *(partition.begin() + ((*ptr_row_idx)-1)) = *ptr_v++;
        
        if(ptr_v == values.end()){
          ptr_v = values.begin();
          ii_v = 0;
        } else {
          ii_v++;
        }
      }
      
    } else {
      // value_len
      for(ptr_partition = partition.begin(); ptr_partition != partition.end(); ptr_partition++){
        *ptr_partition = *ptr_v++;
        
        if(ptr_v == values.end()){
          ptr_v = values.begin();
          ii_v = 0;
        } else {
          ii_v++;
        }
      }
      
    }
    
    // write to file
    if( RTYPE == CPLXSXP){
      tbl = List::create(
        _["V1R"] = Re(static_cast<ComplexVector>(partition)),
        _["V1I"] = Im(static_cast<ComplexVector>(partition))
      );
    } else {
      tbl = List::create(_["V1"] = partition);
    }
    fstStore(fstfile, tbl, wrap(compression), wrap(uniformEncoding));
    
  }
  if(partition_loc == R_MissingArg){
    return(wrap(-1));
  }
  return partition_loc;
}

// SEXP writeFstPartition_double(const Rcpp::NumericVector& values, const Rcpp::StringVector& files, 
//                               const Rcpp::NumericVector& dim, const Rcpp::List& subparsed,
//                               SEXP compression, SEXP uniformEncoding)
// SEXP subsetFST(Rcpp::StringVector& files, SEXP listOrEnv, Rcpp::NumericVector& dim, 
//                SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false)

SEXP subsetAssignFST(const SEXP values, const std::string& file, SEXP listOrEnv,
                     const std::vector<int64_t>& dim, const SEXPTYPE& dtype,
                     int compression, bool uniformEncoding){
  List subparsed = parseSlices(listOrEnv, dim);
  // const int subset_mode = subparsed["subset_mode"];
  
  // // If subset_mode == 1 - x[i]
  // if(subset_mode == LASUBMOD_SINGLE){
  //   List location_indices = subparsed["location_indices"];
  //   
  //   
  // }
  const std::string file_alt = as_dirpath(file);
  SEXP partition_changed;
  
  // riteFstPartition ILi13EEP7 SEXPREC RKN4 Rcpp6Vector IXT_ENS2_15 PreserveStorage EEERKNSt3__112 basic_stringIcNS8_11 char_traitsIcEENS8_9 allocatorIcEEEERKNS8_6 vectorIxNSC_I xEEEERKNS3_I Li19ES4_EEib
  
  switch(dtype){
  case REALSXP: {
    partition_changed = writeFstPartition<REALSXP>(as<NumericVector>(values), file_alt, dim, subparsed, compression, uniformEncoding);
    break;
  }
  case INTSXP: {
    partition_changed = writeFstPartition<INTSXP>(as<IntegerVector>(values), file_alt, dim, subparsed, compression, uniformEncoding);
    break;
  }
  case CPLXSXP:{
    partition_changed = writeFstPartition<CPLXSXP>(as<ComplexVector>(values), file_alt, dim, subparsed, compression, uniformEncoding);
    break;
  }
  case STRSXP:{
    partition_changed = writeFstPartition<STRSXP>(as<StringVector>(values), file_alt, dim, subparsed, compression, uniformEncoding);
    break;
  }
  default:
    stop("Unknown data type, only integer(13), double(14), complex(15), string(16) are allows");
  };
  
  return partition_changed;
  
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
