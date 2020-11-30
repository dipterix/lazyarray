#ifndef API_LAZYARRAY_BMCLASS_H
#define API_LAZYARRAY_BMCLASS_H

#include "entry.h"
#include "LazyArrayBase.h"

namespace lazyarray {

class BMArray : public LazyArrayBase {
  
  // Constructors and field getter/setter
public:
  BMArray(
    BigMatrix *pMat, std::vector<int64_t> dimension, SEXPTYPE dataType
  ): 
  LazyArrayBase(dimension, dataType), _xpMat( pMat )
  {
    validate();
  }
  
  // To be used by Rcpp modules
  virtual ~BMArray(){}
  
  // methods
  bool validate(bool stopIfError = true) override {
    
    // Rcpp::XPtr<BigMatrix> _xpMat(_addr);
    
    //_nparts _totalLen fstFiles dimension
    bool isValid = true;
    isValid = isValid && stopIfNot(_dimension.size() >= 2, "BMArray must dimension >= 2", stopIfError);
    isValid = isValid && stopIfNot(*(_dimension.end() - 1) == _nparts, "BMArray dimensions inconsistent with number of partitions", stopIfError);
    
    
    int64_t ncols = _xpMat->ncol();
    int64_t nrows = _xpMat->nrow();
    isValid = isValid && stopIfNot(ncols == _nparts, "BMArray internal matrix column inconsistent with number of partitions", stopIfError);
    
    int64_t expectedLen = std::accumulate(_dimension.begin(), _dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    isValid = isValid && stopIfNot((expectedLen == _totalLen) && (nrows * ncols == _totalLen), 
                                   "BMArray file counts inconsistent with number of partitions", stopIfError);
    
    // _dataType 13:int, 14:double, 15: complex, 16: string
    isValid = isValid && stopIfNot(
      _dataType == INTSXP || _dataType == REALSXP || _dataType == STRSXP,
      "BMArray data type invalid. Supported are: int(13), double(14), string(16)", stopIfError);
    
    return isValid;
  }
  
  
  SEXP subset(SEXP listOrEnv, SEXP reshape = R_NilValue, bool drop = false) override {
    NumericVector dim = getDim();
    Rcpp::XPtr<BigMatrix> ptr(_xpMat);
    return subsetBM(Rcpp::Shield<SEXP>(Rcpp::wrap(ptr)), listOrEnv, dim, _dataType, reshape, drop);
    // return R_NilValue;
  };
  
private:
  BigMatrix* _xpMat;
};

}

#endif // API_LAZYARRAY_BMCLASS_H
