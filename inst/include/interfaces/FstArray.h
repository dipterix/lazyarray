#ifndef API_LAZYARRAY_FSTCLASS_H
#define API_LAZYARRAY_FSTCLASS_H

#include "entry.h"
#include "LazyArrayBase.h"

namespace lazyarray {

class FstArray : public LazyArrayBase {
  
  // Constructors and field getter/setter
public:
  FstArray(
    const Rcpp::StringVector& partitionFiles, std::vector<int64_t> dimension, 
    SEXPTYPE dataType, int& compression, bool& uniformEncoding
  ): 
  LazyArrayBase(dimension, dataType), _compression(compression), _uniformEncoding(uniformEncoding)
  {
    fstFiles = Rcpp::StringVector(partitionFiles.begin(), partitionFiles.end());
    validate();
  }
  
  virtual ~FstArray(){}
  
  Rcpp::StringVector fstFiles;
  // std::vector<int64_t> dimension;
  
  // methods
  bool validate(bool stopIfError = true) override {
    //_nparts _totalLen fstFiles dimension
    bool isValid = true;
    isValid = isValid && stopIfNot(_dimension.size() >= 2, "FstArray must dimension >= 2", stopIfError);
    isValid = isValid && stopIfNot(*(_dimension.end() - 1) == _nparts, "FstArray dimensions inconsistent with number of partitions", stopIfError);
    isValid = isValid && stopIfNot(fstFiles.size() == _nparts, "FstArray file counts inconsistent with number of partitions", stopIfError);
    
    int64_t expectedLen = std::accumulate(_dimension.begin(), _dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    isValid = isValid && stopIfNot(expectedLen == _totalLen, "FstArray file counts inconsistent with number of partitions", stopIfError);
    
    // _dataType 13:int, 14:double, 15: complex, 16: string
    isValid = isValid && stopIfNot(
      _dataType == INTSXP || _dataType == REALSXP || _dataType == CPLXSXP || _dataType == STRSXP,
      "FstArray data type invalid. Supported are: int(13), double(14), complex(15), string(16)", stopIfError);
    
    return isValid;
  }
  
  
  SEXP subset(SEXP listOrEnv, SEXP reshape = R_NilValue, bool drop = false) override {
    NumericVector dim = getDim();
    return subsetFST(fstFiles, listOrEnv, dim, _dataType, reshape, drop);
  };
  
  SEXP subsetAssign(SEXP values, SEXP listOrEnv) override {
    NumericVector dim = getDim();
    subsetAssignFST(values, fstFiles, listOrEnv, dim, _dataType, _compression, _uniformEncoding);
    return R_NilValue;
  }
  
protected:
  int  _compression;
  bool _uniformEncoding;
  
};


}


#endif // API_LAZYARRAY_FSTCLASS_H
