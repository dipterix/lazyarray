#include "reshape.h"

#include "common.h"
#include "indexConvert.h"

using namespace Rcpp; 

template <class T, typename I>
List cpp_array_to_list_template(T x, int64_t nrows, int64_t ncols){
  // length of x has been checked so assume length(x) = nrows * ncols
  List re = List::create();
  String colname;
  I ptr1_x = x.begin();
  I ptr2_x = x.begin();
  
  
  for(int64_t ii = 0; ii < ncols; ii++ ){
    ptr2_x += nrows;
    colname = "V" + std::to_string(ii + 1);
    T slice = T(ptr1_x, ptr2_x);
    re.push_back(slice, colname);
    ptr1_x = ptr2_x;
  }
  
  
  return re;
}


List cpp_array_to_list_complex(ComplexVector x, int64_t nrows, int64_t ncols){
  
  
  List re = List::create();
  String colname;
  ComplexVector::iterator ptr1_x = x.begin();
  ComplexVector::iterator ptr2_x = x.begin();
  
  for(int64_t ii = 0; ii < ncols; ii++ ){
    ptr2_x += nrows;
    const ComplexVector slice = ComplexVector(ptr1_x, ptr2_x);
    
    colname = "V" + std::to_string(ii + 1) + "R";
    re.push_back(Armor<NumericVector>(Rcpp::Re(slice)), colname);
    
    colname = "V" + std::to_string(ii + 1) + "I";
    re.push_back(Armor<NumericVector>(Rcpp::Im(slice)), colname);
    
    ptr1_x = ptr2_x;
  }
  
  return re;
}


/**
 * Conver vector x to data.frame with dimension c(first_dim, last_dim)
 * 
 * 2020-09-02: 
 * 1. renamed from cpp_array_to_list to arr2df
 * 2. changed argument, explicitly add nrows and ncols to be memory efficient
 * 3. Added length check
 */
Rcpp::List arr2df(SEXP x, int64_t nrows, int64_t ncols){
  // User explicitly tells which storage type of x should be
  // 9	CHARSXP	internal character strings
  // 10	LGLSXP	logical vectors
  // 13	INTSXP	integer vectors
  // 14	REALSXP	numeric vectors
  // 15	CPLXSXP	complex vectors
  // 16	STRSXP	character vectors
  // 24	RAWSXP	raw vector
  // 
  
  
  // check length of x
  if( nrows * ncols - Rf_xlength(x) != 0 ){
    stop("Cannot reshape array to data.frame, dimension not match");
  }
  
  Rcpp::List re; 
  switch (TYPEOF(x)) {
  case STRSXP:
  case CHARSXP:
    re = cpp_array_to_list_template<CharacterVector, CharacterVector::iterator>(x, nrows, ncols);
    break;
  case LGLSXP:
    re = cpp_array_to_list_template<LogicalVector, LogicalVector::iterator>(x, nrows, ncols);
    break;
  case INTSXP:
    re = cpp_array_to_list_template<IntegerVector, IntegerVector::iterator>(x, nrows, ncols);
    break;
  case REALSXP:
    re = cpp_array_to_list_template<NumericVector, NumericVector::iterator>(x, nrows, ncols);
    break;
  case CPLXSXP:
    re = cpp_array_to_list_complex(x, nrows, ncols);
    break;
  default:
    Rcpp::stop("Unsupported data type, only logical, numeric, complex, character types are supported.");
  }
  return re;
}

