#ifndef API_LAZYARRAY_FSTCLASS_H
#define API_LAZYARRAY_FSTCLASS_H

#include <string>
#include "LazyArrayBase.h"

namespace lazyarray {

class FstArray : public LazyArrayBase {
  
  // Constructors and field getter/setter
public:
  FstArray(
    const std::string rootPath, std::vector<int64_t> dimension, 
    SEXPTYPE dataType, int& compression, bool& uniformEncoding
  ): 
  LazyArrayBase(dimension, dataType), 
  _compression(compression), _uniformEncoding(uniformEncoding)
  {
    if(rootPath.size() == 0){
      _rootPath = "./";
    } else {
      std::string ending = "/";
      if(std::equal(ending.rbegin(), ending.rend(), rootPath.rbegin())){
        _rootPath = rootPath;
      } else {
        _rootPath = rootPath + ending;
      }
    }
    validate();
  }
  
  virtual ~FstArray(){ destroy(); }
  
  // methods
  inline bool validate(bool stopIfError = true) {
    //_nparts _totalLen fstFiles dimension
    bool isValid = LazyArrayBase::validate(stopIfError);
    isValid = isValid && stopIfNot(
      _dataType == INTSXP || _dataType == REALSXP || _dataType == CPLXSXP || _dataType == STRSXP,
      "FstArray/FstMatrix data type invalid. Supported are: int(13), double(14), complex(15), string(16)", stopIfError);
    return isValid;
  }
  
  inline SEXP subset(SEXP listOrEnv, SEXP reshape = R_NilValue, bool drop = false) override {
    tok("S subset");
    Rcpp::NumericVector dim = int64t2NumericVector(_dimension);
    SEXP res = subsetFST(_rootPath, listOrEnv, dim, _dataType, reshape, drop);;
    tok("E subset");
    return res;
  };
  
  inline SEXP subsetAssign(SEXP values, SEXP listOrEnv) override {
    tok("S subsetAssign");
    Rcpp::NumericVector dim = int64t2NumericVector(_dimension);
    subsetAssignFST(values, _rootPath, listOrEnv, dim, _dataType, _compression, _uniformEncoding);
    tok("E subsetAssign");
    return R_NilValue;
  }
  
  inline Rcpp::StringVector get_partition_path(SEXP part = R_NilValue){
    tok("S get_partition_path");
    Rcpp::StringVector fstFiles;
    if(Rf_isNull(part)){
      fstFiles = Rcpp::StringVector(_nparts);
      Rcpp::StringVector::iterator ptr_fstFiles = fstFiles.begin();
      for(int64_t ii = 1; ptr_fstFiles != fstFiles.end(); ii++, ptr_fstFiles++){
        *ptr_fstFiles = _rootPath + std::to_string(ii) + ".fst";
      }
    } else {
      std::vector<int64_t> part_alt = as<std::vector<int64_t>>(part);
      fstFiles = Rcpp::StringVector(part_alt.size());
      Rcpp::StringVector::iterator ptr_fstFiles = fstFiles.begin();
      std::vector<int64_t>::iterator ptr_part_alt = part_alt.begin();
      for(; ptr_fstFiles != fstFiles.end(); ptr_part_alt++, ptr_fstFiles++){
        *ptr_fstFiles = _rootPath + std::to_string(*ptr_part_alt) + ".fst";
      }
    }
    tok("E get_partition_path");
    return fstFiles;
  }
  
  
protected:
  int  _compression;
  bool _uniformEncoding;
  std::string _rootPath;


};


}


#endif // API_LAZYARRAY_FSTCLASS_H
