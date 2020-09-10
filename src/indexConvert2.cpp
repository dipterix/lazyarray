#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "indexConvert.h"
using namespace Rcpp;



// [[Rcpp::export]]
List scheduleIndexing(List locations, SEXP dimension) {
  std::vector<int64_t> dim = sexp2Int64tVec(dimension);
  R_xlen_t ndims = dim.size();
  
  stopIfNot(
    locations.size() == ndims,
    "`scheduleIndexing`: locations and dim have different sizes"
  );
  
  // Find split
  // dim -> dim1 x dim2 length(dim1) <= length(dim)-1
  // std::vector<int64_t> dim_int64 = numeric2Int64t(dim);
  int64_t block_size = 1;
  R_xlen_t buffer_margin = 0;
  for(buffer_margin = 0; buffer_margin < ndims - 1; buffer_margin++ ){
    if(block_size > getLazyBlockSize()){
      break;
    }
    block_size *= dim[buffer_margin];
  }
  
  // sub-block size is buffer_margin
  std::vector<int64_t> block_dim = std::vector<int64_t>(dim.begin(), dim.begin() + buffer_margin);
  std::vector<int64_t> rest_dim = std::vector<int64_t>(dim.begin() + buffer_margin, dim.end());
  std::vector<std::vector<int64_t>> rest_loc(ndims - buffer_margin);
  
  for(R_xlen_t ii = buffer_margin; ii < ndims; ii++){
    
  }
  
  print(wrap(buffer_margin));
  print(wrap((int)(block_size)));
  stop("");
  // int64_t nblocks = 1;
  
  
  int64_t expected_length = 1;
  NumericVector target_dimension = NumericVector(dim.begin(), dim.end());
  for(R_xlen_t ii = 0; ii < ndims; ii++ ){
    SEXP loc_ii = locations[ii];
    
    expected_length *= Rf_xlength( loc_ii );
    target_dimension[ii] = Rf_xlength( loc_ii );
  }
  
  List re = List::create(
    _["dimension"] = wrap(dim),
    _["expected_length"] = expected_length,
    _["target_dimension"] = target_dimension
  );
  return re;
  
  // Split dimension by two: 
  
  
  
  
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# scheduleIndexing(list(1,1,1,1), c(287,200,601,84))
# scheduleIndexing(list(1,1,1,1), c(1,2,3,4))
scheduleIndexing(list(1,1), c(9223372036854775806, 2))
scheduleIndexing(list(1,1), c(2L, 2L))
scheduleIndexing(list(1,1,1), c(1,9223372036854775806, 2))
*/
