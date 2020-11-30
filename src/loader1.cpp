#include "loader1.h"
// [[Rcpp::plugins("cpp11")]]


#include "common.h"
#include "indexConvert.h"
#include "fstWrapper.h"
using namespace Rcpp; 


template <typename T, typename I>
SEXP lazyLoadOld_base_internal(
    StringVector& files, IntegerVector& first_indices, IntegerVector& second_indices){
  
  // Rcpp::print(files);
  // Rcpp::print(first_indices);
  // Rcpp::print(second_indices);
  // Rcpp::print(target_dim);
  // Rcout << n_rows << " " << n_cols << " " << subset_nrows << " \n";
  
  // calculate subset dimension for each file
  R_xlen_t subset_nrows = first_indices.size();
  R_xlen_t subset_ncols = second_indices.size();
  
  // The block size is array size to be read for each file (partition)
  R_xlen_t block_size = subset_nrows * subset_ncols;
  
  // initialize result object
  T re = no_init(block_size * files.size());
  
  I ptr_first = re.begin();
  IntegerVector::iterator ptr_idx = first_indices.begin();
  
  // Get start and end rows to optimize _fstcore_fstretrieve
  R_len_t start = Rcpp::min(na_omit(first_indices));
  R_len_t end = Rcpp::max(na_omit(first_indices));
  
  // No need to check whether end overflows as the _fstcore_fstretrieve automatically trims the end
  // if( end > n_rows ){ end = n_rows; }
  // return R_NilValue;
  
  String colname, fileName;
  List tmp;
  T buffer;
  
  List meta;
  
  // For each files, read data and slice them into blocks
  for(StringVector::iterator ptr_f = files.begin(); ptr_f != files.end(); ptr_f++ ){
    fileName = *ptr_f;
    // get meta information
    meta = fstMeta(fileName);
    // print(meta.attr("class"));
    
    // It's possible that the file is missing or correputed, in this case, fill the chunks with NA
    if( Rf_inherits(meta, "fst_error") || 
        !meta.containsElementNamed("nrOfRows") ||
        !meta.containsElementNamed("nrOfCols")){
        // this file is invalid, fill with na
        std::fill_n(ptr_first, block_size, T::get_na());
      ptr_first += block_size;
      // Rcout << "continued" << "\n";
      continue;
    }
    
    R_xlen_t n_cols = as<R_xlen_t>(meta["nrOfCols"]);
    
    // Rcout << "meta valid" << "\n";
    // Rcpp::print(meta);
    
    // The read data is a data.frame, need to handle for each columns
    // For each column, assign to block
    for(IntegerVector::iterator ptr_sec = second_indices.begin(); 
        ptr_sec != second_indices.end(); ptr_sec++ ){
      
      // If column index is NA or invalid, assign NAs to block
      if(*ptr_sec == NA_INTEGER || *ptr_sec < 1 || *ptr_sec > n_cols){
        std::fill_n(ptr_first, subset_nrows, T::get_na());
        ptr_first += subset_nrows;
      } else{
        colname = "V" + std::to_string( *ptr_sec );
        tmp = fstRetrieve(fileName, wrap(colname), wrap(start), wrap(end));
        
        // Read error, just fill with NAs
        if( Rf_inherits(tmp, "fst_error") || 
            !tmp.containsElementNamed("resTable") ){
            // this file is invalid, fill with na
            std::fill_n(ptr_first, subset_nrows, T::get_na());
          ptr_first += subset_nrows;
          continue;
        }
        
        
        // Data retrieved! assign
        tmp = tmp["resTable"];
        buffer = wrap(tmp[colname]);
        
        
        end = start - 1 + buffer.size();
        
        for(ptr_idx = first_indices.begin(); ptr_idx != first_indices.end(); ptr_idx++, ptr_first++){
          // Rcout << (*ptr_idx >= end) << "\n";
          if(*ptr_idx == NA_INTEGER || *ptr_idx < 1 || *ptr_idx > end ){
            // NA
            *ptr_first = T::get_na();
            // *ptr_first = na_value[0];
          } else {
            // buffer[*ptr_idx - start ]
            *ptr_first = buffer[*ptr_idx - start ];
          }
        }
        
      }
    }
  }
  
  return re;
}


ComplexVector lazyLoadOld_base_complex(
    StringVector& files, IntegerVector& first_indices, IntegerVector& second_indices){
  
  // calculate subset dimension for each file
  R_xlen_t subset_nrows = first_indices.size();
  R_xlen_t subset_ncols = second_indices.size();
  
  // The block size is array size to be read for each file (partition)
  R_xlen_t block_size = subset_nrows * subset_ncols;
  
  ComplexVector re = no_init(block_size * files.size());
  ComplexVector::iterator ptr_first = re.begin();
  IntegerVector::iterator ptr_idx = first_indices.begin();
  
  // Get start and end rows to optimize _fstcore_fstretrieve
  R_len_t start = Rcpp::min(na_omit(first_indices));
  R_len_t end = Rcpp::max(na_omit(first_indices));
  
  String colname;
  List tmp;
  NumericVector real, imag;
  NumericVector::iterator ptr_real = real.begin(), ptr_imag = imag.begin();
  std::string buf;
  
  List meta;
  
  for(StringVector::iterator ptr_f = files.begin(); ptr_f != files.end(); ptr_f++ ){
    String fileName = *ptr_f;
    // get meta information
    meta = fstMeta(fileName);
    if( Rf_inherits(meta, "fst_error") || 
        !meta.containsElementNamed("nrOfRows") ||
        !meta.containsElementNamed("nrOfCols") ){
        // this file is invalid, fill with na
        std::fill_n(ptr_first, block_size, ComplexVector::get_na());
      ptr_first += block_size;
      continue;
    }
    
    R_xlen_t n_cols = as<R_xlen_t>(meta["nrOfCols"]);
    
    
    for(IntegerVector::iterator ptr_sec = second_indices.begin(); 
        ptr_sec != second_indices.end(); ptr_sec++ ){
      
      
      if(*ptr_sec == NA_INTEGER || *ptr_sec < 1 || *ptr_sec > n_cols){
        std::fill_n(ptr_first, subset_nrows, ComplexVector::get_na());
        ptr_first += subset_nrows;
      } else{
        
        
        // Read Real and Imaginary columns
        buf = "V" + std::to_string( *ptr_sec ) + "R";
        tmp = fstRetrieve(fileName, wrap(buf), wrap(start), wrap(end));
        
        if( Rf_inherits(tmp, "fst_error") || 
            !tmp.containsElementNamed("resTable") ){
            // this file is invalid, fill with na
            std::fill_n(ptr_first, subset_nrows, ComplexVector::get_na());
          ptr_first += subset_nrows;
          continue;
        }
        
        tmp = tmp["resTable"];
        
        
        real = Shield<NumericVector>(wrap(tmp[buf]));
        
        buf = "V" + std::to_string( *ptr_sec ) + "I";
        tmp = fstRetrieve(fileName, wrap(buf), wrap(start), wrap(end));
        
        if( Rf_inherits(tmp, "fst_error") || 
            !tmp.containsElementNamed("resTable") ){
            // this file is invalid, fill with na
            std::fill_n(ptr_first, subset_nrows, ComplexVector::get_na());
          ptr_first += subset_nrows;
          continue;
        }
        
        
        tmp = tmp["resTable"];
        imag = Shield<NumericVector>(wrap(tmp[buf]));
        
        ptr_real = real.begin();
        ptr_imag = imag.begin();
        end = start - 1 + real.size();
        
        
        for(ptr_idx = first_indices.begin(); ptr_idx != first_indices.end(); ptr_idx++, ptr_first++){
          // Rcout << (*ptr_idx >= end) << "\n";
          if(*ptr_idx == NA_INTEGER || *ptr_idx < 1 || *ptr_idx > end ){
            // NA
            *ptr_first = ComplexVector::get_na();
            // *ptr_first = na_value[0];
          } else {
            // buffer[*ptr_idx - start ]
            (*ptr_first).r = *(ptr_real + *ptr_idx - start);
            (*ptr_first).i = *(ptr_imag + *ptr_idx - start);
          }
        }
        
        
      }
    }
  }
  
  return re;
}

/**
 * @param files files to be read from
 * @param partition_dim underlying dimension for each file
 * @param target_dim result dimension
 * @param first_indices row indices of fst table
 * @param second_indices collumn indices of fst table
 * @param type R type to convert to
 */
SEXP lazyLoadBaseOld(
    StringVector& files, IntegerVector& partition_dim, IntegerVector& target_dim,
    IntegerVector& first_indices, IntegerVector& second_indices,
    int type){
  // SEXP columnSelection, SEXP startRow, SEXP endRow
  // return fstRetrieve(fileName, columnSelection, startRow, endRow);
  
  if(files.size() == 0){
    // it's garanteed, but just in case
    stop("file length is zero.");
  }
  if(partition_dim.size() < 2){
    stop("Invalid partition dimension.");
  }
  
  // get meta information
  // List meta = fstMeta(files[0]);
  
  // R_xlen_t n_cols = meta["nrOfCols"];
  // R_xlen_t n_cols = *(partition_dim.end() - 1);
  // R_xlen_t n_rows = meta["nrOfRows"];
  // R_xlen_t n_rows = std::accumulate(partition_dim.begin(), partition_dim.end() - 1, INTEGER_XLEN_ONE, std::multiplies<R_xlen_t>());
  
  // IntegerVector first_indices_copy = IntegerVector( first_indices );
  // R_xlen_t subset_nrows = first_indices_copy.size();
  // R_xlen_t n_cols_sub = second_indices.size();
  
  SEXP re;
  
  switch( type ){
  case STRSXP:
    re = lazyLoadOld_base_internal<StringVector, StringVector::iterator>(
      files, first_indices, second_indices);
    break;
  case CHARSXP:
    re = lazyLoadOld_base_internal<CharacterVector, CharacterVector::iterator>(
      files, first_indices, second_indices);
    break;
  case LGLSXP:
  case RAWSXP:
  case INTSXP:
    re = lazyLoadOld_base_internal<IntegerVector, IntegerVector::iterator>(
      files, first_indices, second_indices);
    break;
  case REALSXP:
    re = lazyLoadOld_base_internal<NumericVector, NumericVector::iterator>(
      files, first_indices, second_indices);
    break;
  case CPLXSXP: {
    re = lazyLoadOld_base_complex(files, first_indices, second_indices);
    break;
  }
  default:
    Rcpp::stop("Unsupported data type. Only logical, numeric, complex, character types are supported.");
  }
  
  Rf_setAttrib(re, wrap("dim"), wrap(target_dim));
  
  return re;
}


/**
 * For example array of dimension c(10,100,20)
 * @param files files to load from
 * @param partition_locations
 * @param partition_dim partition dimension, e.g. c(10,100,1), or c(10,100), depending on how partition is stored
 * @param ndim total number of dimensions of array, in this example it's 3
 * @param value_type value type of numbers stored
 */
SEXP lazyLoadOld(StringVector& files, List& partition_locations, 
                        IntegerVector& partition_dim, R_xlen_t ndim,  SEXP value_type){
  // files are the total files
  // locations is to each partition
  // dim is total dim
  
  // Get partition dimension size
  R_xlen_t part_dim = partition_dim.size();
  if( part_dim != partition_locations.size() || part_dim < 2 ){
    stop("Dimension not match for lazyLoadOld");
  }
  
  // Obtain the dimension to be returned
  IntegerVector target_dim = sapply(partition_locations, Rf_length);
  
  // If dim for each partition shares the same length as array dim,
  if(part_dim == ndim){
    target_dim[part_dim - 1] *= files.size();
  } else {
    // multipart, mode: 2: tensor mode = part mode + 1
    // this mode is experimental
    
    if( ndim - part_dim != 1 ){
      stop("Total dimension must be partition size+1 in multipart mode~2");
    }
    
    target_dim.push_back( files.size() );
  }
  
  // int64_t target_length = std::accumulate(target_dim.begin(), target_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  
  // if( target_length == 0 )
  
  /*
   * split partition_locations into two parts
   * the last of partition dim is column of fst -- column_indices
   * the other dimensions need to be converted to rows of fst -- first_dim & first_loc
   */
  
  // first_dim: partition_dim[-length(partition_dim)], e.g. c(10,100)
  IntegerVector first_dim = IntegerVector(partition_dim.begin(), partition_dim.end() - 1);
  
  // column_indices = partition_dim[length(partition_dim)]
  IntegerVector column_indices = IntegerVector(partition_locations[part_dim - 1]);
  
  // Partition location indexes to be mapped to fst rows
  List first_loc(part_dim - 1);
  for(R_xlen_t ii = 0; ii < part_dim - 1; ii++){
    first_loc[ii] = partition_locations[ii];
  }
  
  
  // TODO: check whether first_len is needed
  int64_t first_len = std::accumulate(target_dim.begin(), target_dim.begin() + part_dim - 1, INTEGER64_ONE, std::multiplies<int64_t>());
  // What if re_len is 0?
  if( first_len == 0 || column_indices.size() == 0 || files.size() == 0 ){
    // return numeric(0)?
    return R_NilValue;
  }
  
  IntegerVector row_indices = loc2idx(first_loc, first_dim);
  
  
  
  // print(partition_dim);
  SEXP re = lazyLoadBaseOld(files, partition_dim, target_dim, 
                        row_indices, column_indices, TYPEOF(value_type));
  return re;
}

/*** R
f = normalizePath(tempfile(), mustWork = F)
x <- rnorm(10000) + 1i * rnorm(10000); dim(x) <- c(10, 20,50)
x[sample(10000, 2000)] = NA
unlink(f)
lazyarray:::cpp_create_lazyarray(x, dim(x), f, 100L, TRUE);

expect_true(file.exists(f), label = "cpp_create_lazyarray can write to file")

expect_true(setequal(names(fst::fst(f)), paste0('V', 1:50, rep(c('R','I'), each = 50))))
idx_loc <- list(
  as.integer(sample(2) - 1),
  as.integer(sample(2)-1),
  as.integer(sample(2)-1)
)
# .Call(fstcore:::`_fstcore_fstretrieve`, f, c('V1R', 'V1I'), 1L, 2L)
x[1,1,1]



y1 <- cpp_load_lazyarray(f, as.list(rep(1L, 3)), dim, 3L, 1i)

# cpp_load_lazyarray_base_complex(f, 1:2, 1:2, c(2L, 2L), 200L, 50L, 2L, 2L, 15L)


*/
