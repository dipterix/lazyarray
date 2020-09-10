#include <Rcpp.h>
// [[Rcpp::depends(BH, bigmemory)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <numeric>
#include "indexConvert.h"
using namespace Rcpp;

// Logic for BigColSums.
template <typename T>
NumericVector BigColSums(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat) {
  
  // Create the vector we'll store the column sums in.
  NumericVector colSums(pMat->ncol());
  for (size_t i=0; i < pMat->ncol(); ++i)
    colSums[i] = std::accumulate(mat[i], mat[i]+pMat->nrow(), 0.0);
  return colSums;
}

// Dispatch function for BigColSums
//
// [[Rcpp::export]]
NumericVector BigColSums(SEXP pBigMat) {
  // First we have to tell Rcpp what class to use for big.matrix objects.
  // This object stores the attributes of the big.matrix object passed to it
  // by R.
  XPtr<BigMatrix> xpMat(pBigMat);
  
  // To access values in the big.matrix, we need to create a MatrixAccessor
  // object of the appropriate type. Note that in every case we are still
  // returning a NumericVector: this is because big.matrix objects only store
  // numeric values in R, even if their type is set to 'char'. The types
  // simply correspond to the number of bytes used for each element.
  switch(xpMat->matrix_type()) {
  case 1:
    return BigColSums(xpMat, MatrixAccessor<char>(*xpMat));
  case 2:
    return BigColSums(xpMat, MatrixAccessor<short>(*xpMat));
  case 4:
    return BigColSums(xpMat, MatrixAccessor<int>(*xpMat));
  case 8:
    return BigColSums(xpMat, MatrixAccessor<double>(*xpMat));
  default:
    // This case should never be encountered unless the implementation of
    // big.matrix changes, but is necessary to implement shut up compiler
    // warnings.
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}
/*** R
bigm <- bigmemory::attach.resource(file.path('~/Desktop/junk/', 'bigmemory-ieeg.testfile.desc'))
lazy <- lazyarray('~/Desktop/lazyarray_data/')
idx <- lazyarray:::loc2idx2(list(1,1,1,1:2), dim(lazy))

# Call the Rcpp function.
res <- BigColSums(bigm@address) 
print(res)
*/
