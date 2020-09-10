#ifndef LAZYARRAY_CLASS_H
#define LAZYARRAY_CLASS_H

# include <Rcpp.h>
#include "common.h"
#include "utils.h"
using namespace Rcpp;

class FstLazyArray {
  
public:
  enum DataType {DOUBLE=REALSXP};
  
  // Constructors and field getter/setter
public:
  FstLazyArray(
    std::vector<std::string> fstFiles, std::vector<int64_t> dimension, int dataType
  ): 
  fstFiles(fstFiles), dimension(dimension), _dataType(dataType), _readOnly(true)
  {
    _nparts = *(dimension.end() - 1);
    _totalLen = std::accumulate(dimension.begin(), dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    _partLen = _totalLen / _nparts;
    validate();
  }
  
  virtual ~FstLazyArray(){}
  
  std::vector<std::string> fstFiles;
  std::vector<int64_t> dimension;
  
  // getter
  int64_t nparts() const { return _nparts; }
  int64_t partLen() const { return _partLen; }
  int dataType() const { return _dataType; }
  const bool readOnly() const { return _readOnly; }
  
  // setter
  void readOnly(const bool isReadOnly){
    _readOnly = isReadOnly;
  }
  
  bool validate(bool stopIfError = true);
  
  
protected:
  
  int64_t              _nparts;
  int64_t              _partLen;
  int64_t              _totalLen;
  int                  _dataType;
  bool                 _readOnly;
};




// Define methods inline

inline bool FstLazyArray::validate(bool stopIfError) {
  //_nparts _totalLen fstFiles dimension
  bool isValid = true;
  isValid = isValid || stopIfNot(dimension.size() >= 2, "FstLazyArray must dimension >= 2", stopIfError);
  isValid = isValid || stopIfNot(*(dimension.end() - 1) == _nparts, "FstLazyArray dimensions inconsistent with number of partitions", stopIfError);
  isValid = isValid || stopIfNot(fstFiles.size() == _nparts, "FstLazyArray file counts inconsistent with number of partitions", stopIfError);
  
  int64_t expectedLen = std::accumulate(dimension.begin(), dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  isValid = isValid || stopIfNot(expectedLen == _totalLen, "FstLazyArray file counts inconsistent with number of partitions", stopIfError);
  return isValid;
}




#endif // LAZYARRAY_CLASS_H

