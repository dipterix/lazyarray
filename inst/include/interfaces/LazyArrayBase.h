#ifndef API_LAZYARRAY_BASECLASS_H
#define API_LAZYARRAY_BASECLASS_H

#include "entry.h"

namespace lazyarray {

  namespace {
    const int64_t INTEGER64_ONE = 1;
    
    
    inline Rcpp::NumericVector int64t2NumericVector(std::vector<int64_t> x){
      Rcpp::NumericVector re = Rcpp::NumericVector(x.begin(), x.end());
      return(re);
    }
  }

class LazyArrayBase {
  
public:
  LazyArrayBase(
    std::vector<int64_t> dimension, SEXPTYPE dataType
  ): 
  _dimension(dimension), _dataType(dataType), _readOnly(true)
  {
    _nparts = *(_dimension.end() - 1);
    _totalLen = std::accumulate(_dimension.begin(), _dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    _partLen = _totalLen / _nparts;
    // validate();
  }
  
  virtual ~LazyArrayBase(){}
  
  // dimension of array
  
  
  // getter
  int64_t nparts() const { return _nparts; }
  int64_t partLen() const { return _partLen; }
  int dataType() const { return static_cast<int>(_dataType); }
  bool readOnly() const { return _readOnly; }
  NumericVector getDim() const { return int64t2NumericVector(_dimension); }
  
  // setter
  void readOnly(const bool isReadOnly){
    _readOnly = isReadOnly;
  }
  
  // abstract methods
  
  virtual SEXP subset(SEXP listOrEnv, SEXP reshape = R_NilValue, bool drop = false) {
    stop("c++: LazyArrayBase::subset not implemented");
  };
  virtual SEXP subsetAssign(SEXP values, SEXP listOrEnv) {
    stop("c++: LazyArrayBase::subsetAssign not implemented");
  }
  
  // pre-defined members
  /**
   * Validate whether the array has errors
   * @param stopIfError if true, stop or raise exceptions when errors are detected
   * @return true or false whether the array passes the validation test
   */
  virtual bool validate(bool stopIfError = true) {
    bool isValid = true;
    isValid = isValid && stopIfNot(_dimension.size() >= 2, "LazyArray must have dimension size >= 2", stopIfError);
    isValid = isValid && stopIfNot(*(_dimension.end() - 1) == _nparts, "LazyArray dimensions inconsistent with number of partitions", stopIfError);
    int64_t expectedLen = std::accumulate(_dimension.begin(), _dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    isValid = isValid && stopIfNot(expectedLen == _totalLen, "LazyArray file counts inconsistent with number of partitions", stopIfError);
    return isValid;
  };
  
  /**
   * Parse indices and prepare the subset
   * @param sliceIdx  a list (VECSXP), its length should match with dimension size,
   *                  and each element should be REALSXP (int64_t) or INTSXP (int),
   *                  or just R_MissingValue indicating the indices to subset. 
   *                  For example, 
   *   x[1,2,3]         - sliceIdx=list(1, 2, 3)
   *   x[, c(0:10,NA)]  - sliceIdx=list(get_missing_value(), c(0:10,NA_real_))
   *   x[]              - sliceIdx=list() or list(get_missing_value())
   *   x[-(1,2)]        - sliceIdx=list(c(-1,-1))
   * @return A list containing parsed information:
   *   - subset_mode (int): subset mode: 0 means x[i,j,k], 1 means x[i], 2 means x[]
   *   - target_dimension (std::vector<int64_t> or NumericVector): expected dimension of results
   *   - expected_length (int64_t): expected result length (= prod(target_dimension))
   *   - negative_subscript: whether this margin should be negative sliced (currectly all false)
   *   - location_indices: a list of cleaned sliceIdx. For example:
   *       [expression]      => [location_indices]
   *       x[1,2,3]          => list(1,2,3)
   *       x[, c(0:10,NA)]   => list({R_MissingValue}, c(1:10,NA_REAL))
   *       x[]               => list({R_MissingValue})
   *       x[-(0:1),2]       => list(2:3, 2), suppose dimension is c(3,4)
   *   - schedule: a list or R_NilValue. If subset=0, schedule will schedule indexing
   *                  blocks to read from array. 
   */
  inline Rcpp::List scheduleBlocks(SEXP sliceIdx) {
    NumericVector dim = getDim();
    return parseAndScheduleBlocks(sliceIdx, dim);
  };
  
  
  
  
protected:
  
  std::vector<int64_t> _dimension;
  
  // Number of partitions - last dimension
  int64_t              _nparts;
  
  // Partition size
  int64_t              _partLen;
  
  // 
  int64_t              _totalLen;
  SEXPTYPE             _dataType;
  bool                 _readOnly;
};

}

#endif // API_LAZYARRAY_BASECLASS_H
