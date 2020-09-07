#ifndef DIP_LAZYARRAY_COMMON_H
#define DIP_LAZYARRAY_COMMON_H

// Common header that's required by all (most) files

#include <vector>
#include <iostream>
#include <iterator>
#include <complex>
#include <cmath>
#include <cstring>
// #include <string>

#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>

// [[Rcpp::depends(fstcore)]]
#include <fstcore.h>

const bool LAZYARRAY_DEBUG = true;

/*
 * Number of bytes fst uses to compress as a unit
 * We use it differently, basically 4x or 8x or 16x this number as our block size
 * to avoid repeating too many blocks
 */ 
#define NA_INTEGER64 LLONG_MIN


static R_xlen_t BLOCKSIZE = 16384;

#endif // DIP_LAZYARRAY_COMMON_H
