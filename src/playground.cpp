#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
//include "openMPInterface.h"
#include "utils.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector asi(SEXP v, int nt) {
  // NumericVector x(l);
  R_xlen_t l = Rf_xlength(v);
  SEXP x = PROTECT(Rf_allocVector(REALSXP, l));
  double* ptr = REAL(x);
  Rcpp::Timer _rcpp_timer;
  _rcpp_timer.step("start assignment");
  
#pragma omp parallel num_threads(nt)
{
#pragma omp for
  for(int file_idx = 0; file_idx < l; file_idx++ ){
    *(ptr + file_idx) = REAL(v)[file_idx];
  }
} 
  UNPROTECT(1);

  _rcpp_timer.step("finished");
  
  // return NumericVector(data,data+sizeof(data)/sizeof(int));
  
  NumericVector _res(_rcpp_timer);
  _res = _res / 1000000.0;

  return _res;
}

// [[Rcpp::export]]
SEXP playground(int x){
  SEXP i = wrap(seq_len(x));
  return i;
}



/*** R
# devtools::load_all();
# subsetIdx2(list(9223372036854775806, 9223372036854775806), c(9223372036854775806, 9223372036854775806), TRUE)
playground(2)
# (8023372036854775806/4) + (9223372036854775806 / 4)
# dropDimension(matrix(1:16,1))
# asi(rnorm(1000000), 4L)
# asi(rnorm(1000000), 1L)
*/
