#include <iostream>
#include <fstream>
#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
#include "indexConvert.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP r_readBin2(std::string con, int64_t n, int size, int64_t skip = 0, bool check_length = true){
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Rcpp::Function readBin = env["readBin"];
  Rcpp::Function file = env["file"];
  Rcpp::Function close = env["close.connection"];
  SEXP connection = file(Rcpp::Shield<SEXP>(Rcpp::wrap(con)), Rcpp::wrap("rb"));
  if(skip > 0){
    Rcpp::Function seek = env["seek.connection"];
    seek(connection, wrap(skip * size));
  }
  SEXP re = readBin(connection, wrap("raw"), wrap(n * size), wrap(1), wrap(true), wrap("little"));
  // close
  close(connection);
  return re;
}

/*** R
f <- normalizePath('~/Desktop/filearray_data/1.bmat')
a <- r_readBin2(f, 4096, 8L, skip = 14096L)
filem <- filematrix::fm.open("~/Desktop/filearray_data/1", readonly = TRUE)
range(filem[14096+(1:4096),1] - a)

bench::mark({
  a <- r_readBin2(f, 4096, 8L, skip = 14096L)
}, {
  filem[14096+(1:4096),1]
}, check = F)
*/
