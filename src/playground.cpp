#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector input) {
  
  R_xlen_t n = input.size();
  NumericVector output = no_init(n);
  long long* pInput  = (long long*) dataptr(input);
  long long* pOutput = (long long*) dataptr(output);
  for (R_xlen_t i = 0; i < n; i++)
    *pOutput++ = *pInput++ * 2;
  
  output.attr("class") = "integer64";
  return output;
}

/*** R
library(bit64)
object <- as.integer64(1:16)
timesTwo(object)
*/
