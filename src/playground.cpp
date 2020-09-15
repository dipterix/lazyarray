#include <Rcpp.h>
using namespace Rcpp;

template <SEXPTYPE RTYPE>
SEXP timesTwo(Vector<RTYPE> x) {
  return x;
}

// [[Rcpp::export]]
SEXP timesTwo(SEXP input, SEXPTYPE t){
  return timesTwo<REALSXP>(input);
}




/*** R

timesTwo(2^54, 14L)
*/
