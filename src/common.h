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

using namespace Rcpp;

/*
 * Number of bytes fst uses to compress as a unit
 * We use it differently, basically 4x or 8x or 16x this number as our block size
 * to avoid repeating too many blocks
 */ 
#ifdef NA_INTEGER64
#undef NA_INTEGER64
#endif // NA_INTEGER64


#define NA_INTEGER64 LLONG_MIN

const bool LAZYARRAY_DEBUG = true;

/*
 * For array with dimension [287 x 200 x 601 x 84]
 * BLOCKSIZE decides the size of block to read into from fst file
 * because fst internally stores data in blocks, it's recommended to read blocks with size > 16384
 * 
 * by default, this array will be split into 3 parts
 * [287 x 200 x 601 x 84] => [287 x 200] x [601 x 1] x [84]
 * 
 * 84 is # of partition /files
 * for each file, read in sub chunk of length 287 x 200 (> BLOCKSIZE)
 * total number of chunks to read is 601 per file
 * 
 * loc2idx3 calculates indices within each sub-chunks so that it's easy to find then once data is loaded
 * 
 * However, of sub-block is too large, for example [1e30 x 5] matrix, sub-block size is 1e30, loc2idx3 generates too many
 * indices but the indices come with cost of memory (this means super large index set). We wish to calculate 
 * indices on the fly. The boundary is set by BLOCKLARGE.
 * 
 * If # of indices > BLOCKLARGE, then don't pre-generate indices
 * 
 */

// Used to partition to sub-blocks
static R_xlen_t BLOCKSIZE = 16384;
// If sub-block size is too large, don't calculate indices (memory inefficient)
// ~ 500 MB index set
static R_xlen_t BLOCKLARGE = 31250000;

const static int64_t INTEGER64_ONE = 1;
const static R_xlen_t INTEGER_XLEN_ONE = 1;

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
R_xlen_t setLazyBlockSize(R_xlen_t size);

// [[Rcpp::export]]
R_xlen_t getLazyBlockSize();




#endif // DIP_LAZYARRAY_COMMON_H
