#include <Rcpp.h>
using namespace Rcpp;


SEXP int64t2Sexp(const std::vector<int64_t>& x) {
  const R_xlen_t len = x.size();
  SEXP re = PROTECT(Rf_allocVector(REALSXP, len));
  std::memcpy(REAL(re), &(x[0]), len * sizeof(double));
  Rf_setAttrib(re, wrap("class"), wrap("integer64"));
  return re;
}


std::vector<int64_t> numericVector2Int64tVec(const NumericVector& x){
  const R_xlen_t len = x.size();
  std::vector<int64_t> re( len );
  std::memcpy(&(re[0]), &(x[0]), len * sizeof(int64_t));
  return(re);
}

int64_t doubleInt64t(const double& x){
  int64_t re;
  std::memcpy(&(re), &(x), sizeof(int64_t));
  return(re);
}


double int64t2double(const int64_t& x){
  double re;
  std::memcpy(&(re), &(x), sizeof(double));
  return(re);
}

NumericVector int64tVec2NumericVector(const std::vector<int64_t>& x){
  const R_xlen_t len = x.size();
  NumericVector re = no_init( len );
  std::memcpy(&(re[0]), &(x[0]), len * sizeof(double));
  return(re);
}

std::vector<int64_t> sexp2Int64tVec(SEXP x){
  const R_xlen_t len = Rf_xlength(x);
  std::vector<int64_t> re(len);
  if(TYPEOF(x) == REALSXP){
    std::memcpy(&(re[0]), REAL(x), len * sizeof(int64_t));
  }else{
    SEXP y = PROTECT(Rf_coerceVector(x, REALSXP));
    std::memcpy(&(re[0]), REAL(y), len * sizeof(int64_t));
    UNPROTECT(1);
  }
  return re;
}
