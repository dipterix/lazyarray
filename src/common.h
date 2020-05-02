#ifndef DIP_LAZYARRAY_COMMON_H
#define DIP_LAZYARRAY_COMMON_H

#include <vector>
#include <iostream>
#include <iterator>
#include <complex>
#include <cmath>

#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>

//  #include <omp.h>
//# // [[Rcpp::plugins(openmp)]]

// [[Rcpp::depends(fstcore)]]
#include <fstcore.h>

using namespace Rcpp; 

const bool LAZYARRAY_DEBUG = true;

#endif // DIP_LAZYARRAY_COMMON_H
