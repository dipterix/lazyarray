#ifndef RCPP_lazyarray_H_GEN_
#define RCPP_lazyarray_H_GEN_

#include "lazyarray_RcppExports.h"

namespace lazyarray {

namespace {
  const int64_t INTEGER64_ONE = 1;
  
  
  inline Rcpp::NumericVector int64t2NumericVector(std::vector<int64_t> x){
    Rcpp::NumericVector re = Rcpp::NumericVector(x.begin(), x.end());
    return(re);
  }
}

class FstLazyArray {
  
public:
  enum DataType {DOUBLE=REALSXP, INTG=INTSXP, CPLX = CPLXSXP, STRI = STRSXP};
  
  // Constructors and field getter/setter
public:
  FstLazyArray(
    const Rcpp::StringVector& partitionFiles, std::vector<int64_t> dimension, SEXPTYPE dataType
  ): 
  dimension(dimension), _dataType(dataType), _readOnly(true)
  {
    fstFiles = Rcpp::StringVector(partitionFiles.begin(), partitionFiles.end());
    _nparts = *(dimension.end() - 1);
    _totalLen = std::accumulate(dimension.begin(), dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
    _partLen = _totalLen / _nparts;
    validate();
  }
  
  virtual ~FstLazyArray(){}
  
  Rcpp::StringVector fstFiles;
  std::vector<int64_t> dimension;
  
  // getter
  int64_t nparts() const { return _nparts; }
  int64_t partLen() const { return _partLen; }
  int dataType() const { return static_cast<int>(_dataType); }
  const bool readOnly() const { return _readOnly; }
  
  // setter
  void readOnly(const bool isReadOnly){
    _readOnly = isReadOnly;
  }
  
  
  // methods
  bool validate(bool stopIfError = true);
  
  SEXP subsetBare(const List& subparsed, SEXP reshape = R_NilValue, bool drop = false);
  
  SEXP getSubsetIdx2(Rcpp::List sliceIdx, bool pos_subscript = true);
  SEXP getSubsetIdx(Rcpp::Environment env, bool pos_subscript = true);
  
  SEXP subset(SEXP envOrList, SEXP reshape = R_NilValue, bool drop = false);
  
  
protected:
  
  int64_t              _nparts;
  int64_t              _partLen;
  int64_t              _totalLen;
  SEXPTYPE             _dataType;
  bool                 _readOnly;
  
  
};




// Define methods inline

inline bool FstLazyArray::validate(bool stopIfError) {
  //_nparts _totalLen fstFiles dimension
  bool isValid = true;
  isValid = isValid && stopIfNot(dimension.size() >= 2, "FstLazyArray must dimension >= 2", stopIfError);
  isValid = isValid && stopIfNot(*(dimension.end() - 1) == _nparts, "FstLazyArray dimensions inconsistent with number of partitions", stopIfError);
  isValid = isValid && stopIfNot(fstFiles.size() == _nparts, "FstLazyArray file counts inconsistent with number of partitions", stopIfError);
  
  int64_t expectedLen = std::accumulate(dimension.begin(), dimension.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  isValid = isValid && stopIfNot(expectedLen == _totalLen, "FstLazyArray file counts inconsistent with number of partitions", stopIfError);
  
  // _dataType 13:int, 14:double, 15: complex, 16: string
  isValid = isValid && stopIfNot(
    _dataType == INTSXP || _dataType == REALSXP || _dataType == CPLXSXP || _dataType == STRSXP,
    "FstLazyArray data type invalid. Supported are: int(13), double(14), complex(15), string(16)", stopIfError);
  
  return isValid;
}

inline SEXP FstLazyArray::subsetBare(const List& subparsed, SEXP reshape, bool drop) {
  NumericVector dim = int64t2NumericVector(dimension);
  
  // subparsed is a list
  // subset_mode = 0L,                    -- integer: 0 means x[i,j,k], 1 means x[i], 2 means x[]
  // target_dimension = c(1,1,1,1),       -- NumericVector containing int or int64_t
  // expected_length = 1,                 -- int64_t
  // negative_subscript = rep(FALSE, 4),  -- LogicalVector size must equals to target_dimension
  // location_indices = list(1,1,1,1)     -- list of NumericVector(int64_t) or R_MissingValue
  
  // We don't generate subparsed directly, instead, use function
  // const List subparsed = subsetIdx(env, dim, true);
  
  SEXP res = lazySubsetBare(fstFiles, dim, subparsed, _dataType, reshape, drop);
  
  return res;
  
}

inline SEXP FstLazyArray::getSubsetIdx2(Rcpp::List sliceIdx, bool pos_subscript) {
  return subsetIdx2(sliceIdx, int64t2NumericVector(dimension), pos_subscript);
}


inline SEXP FstLazyArray::getSubsetIdx(Rcpp::Environment env, bool pos_subscript){
  
  // function should be called with f(...){ x$getSubsetIdx(environment(), TRUE) }
  SEXP dots = Rf_findVarInFrame(env, R_DotsSymbol);
  SEXP el;
  
  int64_t idx_size = 0;
  R_xlen_t ndims = dimension.size();
  
  Rcpp::List sliceIdx = Rcpp::List::create();
  
  for(; dots != R_NilValue & dots != R_MissingArg; dots = CDR(dots), idx_size++ ){
    
    if(idx_size >= ndims){
      stop("Incorrect subscript dimensions, required: 0, 1, ndim.");
    }
    
    el = CAR(dots);
    sliceIdx.push_back(el);
  }
  return getSubsetIdx2(sliceIdx, pos_subscript);
  
}

inline SEXP FstLazyArray::subset(SEXP envOrList, SEXP reshape, bool drop){
  
  SEXPTYPE intype = TYPEOF(envOrList);
  
  Rcpp::List li;
  switch(intype) {
  case ENVSXP:
    li = getSubsetIdx(envOrList, true);
    break;
  case VECSXP:
    li = getSubsetIdx2(envOrList, true);
    break;
  default:
    Rcpp::stop("Input envOrList must be either a list of indices or an environment");
  }
  
  SEXP re = subsetBare(li, reshape, drop);
  
  return re;
}


}


#endif // RCPP_lazyarray_H_GEN_
