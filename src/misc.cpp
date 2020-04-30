// [[Rcpp::plugins("cpp11")]]

#include "common.h"
#include "utils.h"
#include "misc.h"
using namespace Rcpp;

SEXP cpp_load_lazyarray_base_real(
    Rcpp::String& fileName, IntegerVector& target_dim,
    IntegerVector& first_indices, IntegerVector& second_indices){
  // SEXP columnSelection, SEXP startRow, SEXP endRow
  // return fstcore::fstretrieve(fileName, columnSelection, startRow, endRow);
  // get meta information
  List meta = fstcore::fstmetadata(fileName);
  R_xlen_t n_rows = meta["nrOfRows"];
  R_xlen_t n_cols = meta["nrOfCols"];
  
  R_xlen_t n_rows_sub = first_indices.size();
  R_xlen_t n_cols_sub = second_indices.size();
  
  NumericVector re = no_init(n_rows_sub * n_cols_sub);
  NumericVector::iterator ptr_first = re.begin();
  IntegerVector::iterator ptr_idx = first_indices.begin();
  int start = Rcpp::min(na_omit(first_indices));
  int end = Rcpp::max(na_omit(first_indices));
  if( end > n_rows ){ end = n_rows; }
  String colname;
  List tmp;
  NumericVector buffer;
  
  for(IntegerVector::iterator ptr_sec = second_indices.begin(); 
      ptr_sec != second_indices.end(); ptr_sec++ ){
    
    if(*ptr_sec < 1 || *ptr_sec > n_cols){
      std::fill_n(ptr_first, n_rows_sub, NA_REAL);
      ptr_first += n_rows_sub;
    } else{
      colname = "V" + std::to_string( *ptr_sec );
      tmp = fstcore::fstretrieve(fileName, wrap(colname), wrap(start), wrap(end));
      tmp = tmp["resTable"];
      buffer = wrap(tmp[colname]);
      
      for(ptr_idx = first_indices.begin(); ptr_idx != first_indices.end(); ptr_idx++, ptr_first++){
        // Rcout << (*ptr_idx >= end) << "\n";
        if(*ptr_idx == NA_INTEGER || *ptr_idx < 1 || *ptr_idx > end ){
          // NA
          *ptr_first = NA_REAL;
        } else {
          // buffer[*ptr_idx - start ]
          *ptr_first = buffer[*ptr_idx - start ];
        }
      }
      
    }
  }
  
  
  re.attr("dim") = target_dim;
  
  return re;
  
}
