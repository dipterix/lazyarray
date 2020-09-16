#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
SEXP example_rcpp(SEXP x){
  return r_cast<REALSXP>(x);
}



/*** R
example_rcpp('1')
*/
