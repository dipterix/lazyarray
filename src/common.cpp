#include "common.h"
using namespace Rcpp;

R_xlen_t setBlockSize(R_xlen_t size){
  if( size < 0 ){
    BLOCKSIZE = 16384;
  } else if( size > 0 ){
    BLOCKSIZE = size;
  }
  
  return BLOCKSIZE;
}

R_xlen_t getBlockSize(){
  if(BLOCKSIZE < 1){
    BLOCKSIZE = 16384;
  }
  return BLOCKSIZE;
}
