#include <iostream>
#include <fstream>
#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
#include "indexConvert.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP cpp_readBin2(std::string con, int64_t n, int size, int64_t skip = 0, bool check_length = true){
  char* buffer = new char[n * size];
  std::ifstream input( con, std::ios::binary );
  int64_t fsize = 0;
  int64_t n_byte = n * size;
  try{
    input.setf(std::ios::ios_base::skipws);
    std::filebuf* pbuf = input.rdbuf();
    if(check_length){
      fsize = pbuf->pubseekoff (-skip * size,input.end,input.in);
      if(fsize < size){
        n_byte = 0;
      } else {
        if(fsize < n_byte){
          n_byte = fsize;
        }
        pbuf->pubseekpos (skip * size,input.in);
        pbuf->sgetn (buffer, n_byte);
      }
    } else {
      pbuf->sgetn (buffer, n_byte);
    }
  } catch (...) {
    n_byte = 0;
  }
  input.close();
  
  SEXP re = PROTECT(Rf_allocVector(REALSXP, n_byte / size));
  
  memcpy(REAL(re), buffer, n_byte);
  
  delete[] buffer;
  UNPROTECT(1);
  
  return re;
}

/*** R
f <- normalizePath('~/Desktop/filearray_data/1.bmat')
a <- cpp_readBin2(f, 4096L, 8L, skip = 14096L)
filem <- filematrix::fm.open("~/Desktop/filearray_data/1", readonly = TRUE)
range(filem[14096+(1:4096),1] - a)
*/
