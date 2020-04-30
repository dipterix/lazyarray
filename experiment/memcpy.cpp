#include "common.h"
#include "util.h"

// [[Rcpp::export]]
Rcpp::List cpp_array_to_list_integer(IntegerVector x, IntegerVector cutoff){
  
  Rcpp::Timer _rcpp_timer;
  _rcpp_timer.step("start cpp_array_to_list_integer");
  
  Rcpp::List re(cutoff.size() - 1);
  
  // memory copy x
  R_xlen_t size = x.size();
  int* array = new int[size];
  ::memcpy(array, &(*x.begin()), size * sizeof(int));
  
  _rcpp_timer.step("memory-copy");
  
  // cutoff
  int *p1, *p2;
  p1 = array + cutoff[0] - 1;
  p2 = p1;
  for( R_xlen_t ii = 0; ii < cutoff.size() - 1; ii++ ){
    p2 += cutoff[ii + 1] - cutoff[ii];
    re[ii] = IntegerVector(p1, p2);
    p1 = p2; 
    _rcpp_timer.step("split-" + std::to_string(ii));
  }
  _rcpp_timer.step("split-finished");
  
  // return NumericVector(data,data+sizeof(data)/sizeof(int));
  
  if( LAZYARRAY_DEBUG ){
    
    NumericVector _res(_rcpp_timer);
    _res = _res / 1000.0;
    Rcpp::print(_res);
  }
  
  return re;
}

/*** R
timesTwo(42)
*/
